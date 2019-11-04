{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE OverloadedStrings #-}

module RPKI.RRDP.Update where

import Colog

import           Control.Exception
import           Control.Monad
import           Control.Lens               ((^.))

import Control.DeepSeq (($!!))

import Control.Monad.Catch hiding (try)
import Control.Monad.IO.Unlift

import           Data.Bifunctor             (first, second)
import qualified Data.ByteString.Lazy       as BL
import Data.IORef
import Data.Char (isAlpha)
import qualified Data.List                  as L
import qualified Data.Text                  as T
import qualified Network.Wreq               as WR

import           GHC.Generics

import           RPKI.Domain
import           RPKI.Store.Storage
import           RPKI.RRDP.Parse
import           RPKI.Parse.Parse
import           RPKI.RRDP.Types
import qualified RPKI.Util                  as U
import qualified Data.ByteString.Streaming as Q
import Data.ByteString.Streaming.HTTP

import qualified Crypto.Hash.SHA256      as S256

import System.IO.Temp (withSystemTempFile)
import System.IO.Posix.MMap.Lazy (unsafeMMapFile)
import System.IO (Handle, hClose)

import qualified UnliftIO.Async as Unlift

{- 
    TODO 
    1) Replace IO with some reasonable Monad (MonadIO + MonadMask/MonadUnliftIO/MonadBaseControl).
    2) Maybe return bracketed IO actions instead of exectuting them.
-}
updateRrdpRepo :: Repository 'Rrdp -> 
                (Snapshot -> IO (Maybe SomeError)) ->
                (Delta -> IO (Maybe SomeError)) ->
                IO (Either SomeError (Repository 'Rrdp, Maybe e))
updateRrdpRepo repo@(RrdpRepo repoUri _) handleSnapshot handleDelta = do
    notificationXml <- download repoUri (RrdpE . CantDownloadNotification . show)
    bindRight (notificationXml >>= (first RrdpE . parseNotification)) $ \notification -> 
        bindRight (first RrdpE $ rrdpNextStep repo notification) $ \case
            NothingToDo            -> pure $ Right (repo, Nothing)
            UseSnapshot snapshot   -> fmap (, Nothing) <$> useSnapshot snapshot                            
            UseDeltas sortedDeltas -> useDeltas sortedDeltas notification
    where
        bindRight e f = either (pure . Left) f e 
        
        useSnapshot (SnapshotInfo uri@(URI u) hash) = do
            let tmpFileName = U.convert $ normalize u
            -- Download snapshot to a temporary file and MMAP it to a lazy bytestring 
            -- to minimize the heap. Snapshots can be pretty big, so we don't want 
            -- a spike in heap usage
            withSystemTempFile tmpFileName $ \name fd -> do
                realHash <- downloadToFile uri (RrdpE . CantDownloadSnapshot . show) fd
                bindRight realHash $ \realHash' ->
                    if realHash' /= hash
                        then pure $ Left $ RrdpE $ SnapshotHashMismatch hash realHash'
                        else do
                            -- File has to be closed before it can be opened again ny mmap
                            hClose fd
                            snapshot <- first RrdpE . parseSnapshot <$> unsafeMMapFile name
                            bindRight snapshot $ \s ->
                                maybe (Right $ repoFromSnapshot s) Left <$> handleSnapshot s

        useDeltas sortedDeltas notification = do
            deltas <- U.parallel 10 processDelta sortedDeltas            
            foldM foldDeltas' ([], Nothing) deltas >>= \case 
                (ds, Nothing) -> pure $ Right (repoFromDeltas ds notification, Nothing)
                (_, Just e)   -> pure $ Left e
            where
                foldDeltas' (valids, Just e)   _         = pure (valids, Just e)
                foldDeltas' (valids, Nothing) (Left e')  = pure (valids, Just e')
                foldDeltas' (valids, Nothing) (Right d) =
                    handleDelta d >>= \case 
                        Nothing -> pure (d : valids, Nothing)
                        Just e  -> pure (valids, Just e)

                processDelta (DeltaInfo uri hash serial) = do                     
                    deltaXml <- download uri (RrdpE . CantDownloadDelta . show)                        
                    bindRight deltaXml $ \dXml ->
                        let realHash = U.sha256 dXml
                        in pure $ if realHash /= hash
                            then Left $ RrdpE $ DeltaHashMismatch hash realHash serial
                            else let !d = first RrdpE $ parseDelta dXml in d

        repoFromSnapshot :: Snapshot -> Repository 'Rrdp
        repoFromSnapshot (Snapshot _ sid s _) = RrdpRepo repoUri $ Just (sid, s)

        repoFromDeltas :: [Delta] -> Notification -> Repository 'Rrdp
        repoFromDeltas ds notification = RrdpRepo repoUri $ Just (newSessionId, newSerial)
            where
                newSessionId = sessionId notification
                newSerial = L.maximum $ map (\(Delta _ _ s _) -> s) ds        


download :: MonadIO m => URI -> (SomeException -> e) -> m (Either e BL.ByteString)
download (URI uri) err = liftIO $ do
    r <- try (WR.get $ T.unpack uri)
    pure $ first err $ second (^. WR.responseBody) r


downloadToFile :: MonadIO m =>
                URI -> 
                (SomeException -> err) -> 
                Handle -> 
                m (Either err Hash)
downloadToFile (URI uri) err destinationHandle = 
    liftIO $ first err <$> try go    
    where
        go = do
            req  <- parseRequest $ T.unpack uri
            tls  <- newManager tlsManagerSettings 
            hash <- newIORef S256.init
            withHTTP req tls $ \resp -> 
                Q.hPut destinationHandle $ 
                    Q.chunkMapM (\chunk -> 
                    modifyIORef' hash (`S256.update` chunk) >> pure chunk) $ 
                    responseBody resp
            h' <- readIORef hash        
            pure $ Hash $ S256.finalize h'


data Step = UseSnapshot SnapshotInfo
          | UseDeltas { sortedDeltas :: [DeltaInfo] }
          | NothingToDo
    deriving (Show, Eq, Ord, Generic)

-- Decide what to do next based on current state of the repository
-- and the parsed notification file
rrdpNextStep :: Repository 'Rrdp -> Notification -> Either RrdpError Step
rrdpNextStep (RrdpRepo _ Nothing) Notification{..} = Right $ UseSnapshot snapshotInfo
rrdpNextStep (RrdpRepo _ (Just (repoSessionId, repoSerial))) Notification{..} =
    if  | sessionId /= repoSessionId -> Right $ UseSnapshot snapshotInfo
        | repoSerial > serial  -> Left $ LocalSerialBiggerThanRemote repoSerial serial
        | repoSerial == serial -> Right NothingToDo
        | otherwise ->
            case (deltas, nonConsecutive) of
                ([], _) -> Right $ UseSnapshot snapshotInfo
                (_, []) | next repoSerial < head (map getSerial sortedDeltas) ->
                           -- we are too far behind
                           Right $ UseSnapshot snapshotInfo
                        | otherwise ->
                           Right $ UseDeltas chosenDeltas
                (_, nc) -> Left $ NonConsecutiveDeltaSerials nc
            where
                sortedSerials = map getSerial sortedDeltas
                sortedDeltas = L.sortOn getSerial deltas
                chosenDeltas = filter ((> repoSerial) . getSerial) sortedDeltas

                nonConsecutive = L.filter (\(s, s') -> next s /= s') $
                    L.zip sortedSerials (tail sortedSerials)


getSerial :: DeltaInfo -> Serial
getSerial (DeltaInfo _ _ s) = s

next :: Serial -> Serial
next (Serial s) = Serial $ s + 1

normalize :: T.Text -> T.Text
normalize = T.map (\c -> if isAlpha c then c else '_') 


process :: Storage s => 
            LogAction IO String ->
            Repository 'Rrdp ->
            s ->
            IO (Either SomeError (Repository 'Rrdp, Maybe RrdpError))
process logger repository storage = 
    updateRrdpRepo repository saveSnapshot saveDelta
    where
        saveSnapshot :: Snapshot -> IO (Maybe SomeError)
        saveSnapshot (Snapshot _ _ _ ps) = do
            logger <& "Using snapshot for the repository: " <> show repository
            either Just (const Nothing) . 
                first (StorageE . StorageError . fmtEx) <$> 
                    try (U.boundedFunnel 20 ps objectAsyns writeObject)
            where
                objectAsyns (SnapshotPublish u encodedb64) =
                    Unlift.async $ pure $!! (u, mkObject u encodedb64)
                
                writeObject a = Unlift.wait a >>= \case                        
                    (u, Left e)   -> logger <& (U.convert $ "Couldn't parse object: " <> show e <> ", u = " <> show u)
                    (u, Right st) -> storeObj storage st            

        saveDelta :: Delta -> IO (Maybe SomeError)
        saveDelta (Delta _ _ _ ds) = 
            either Just (const Nothing) . 
                first (StorageE . StorageError . fmtEx) <$> 
                    try (readWriteTx storage $ U.boundedFunnel 20 ds objectAsyns writeObject)
            where
                objectAsyns (DP (DeltaPublish u h encodedb64)) = do 
                    a <- Unlift.async $ pure $!! mkObject u encodedb64
                    pure $ Right (u, h, a)

                objectAsyns (DW (DeltaWithdraw u h)) = pure $ Left (u, h)       
                
                writeObject = \case
                    Left (u, h)           -> delete storage (h, u)
                    Right (u, Nothing, a) -> 
                        Unlift.wait a >>= \case
                            Left e -> logger <& U.convert ("Couldn't parse object: " <> show e)
                            Right (h, st) ->
                                getByHash storage h >>= \case 
                                    Nothing -> storeObj storage (h, st)
                                    Just existing ->
                                        -- TODO Add location
                                        logger <& U.convert ("There's an existing object with hash: " <> show h)
                    Right (u, Just h, a) -> 
                        Unlift.wait a >>= \case
                            Left e -> logger <& U.convert ("Couldn't parse object: " <> show e)
                            Right (h', st) ->
                                getByHash storage h >>= \case 
                                    Nothing -> 
                                        logger <& U.convert ("No object with hash : " <> show h <> ", nothing to replace")
                                    Just existing -> do 
                                        delete storage (h, u)
                                        getByHash storage h' >>= \case 
                                            Nothing -> storeObj storage (h, st)
                                            Just found -> 
                                                -- TODO Add location
                                                logger <& U.convert ("There's an existing object with hash: " <> show h)

        mkObject (URI u) b64 = do
            DecodedBase64 b <- first RrdpE $ decodeBase64 b64 u
            ro <- first ParseE $ readObject (T.unpack u) b    
            pure (getHash ro, toStorable ro)


fmtEx :: SomeException -> T.Text
fmtEx = T.pack . show 