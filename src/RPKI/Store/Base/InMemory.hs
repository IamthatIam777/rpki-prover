{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}


module RPKI.Store.Base.InMemory where

import Control.Concurrent.STM

import qualified Data.ByteString as BS

import Data.Foldable (for_)
import Data.Maybe (fromMaybe)

import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import RPKI.Store.Base.Storable
import RPKI.Store.Base.Storage

import Data.IORef

import GHC.TypeLits



-- | Simple in-memory storage that is use for testing and ignores 
-- all the transactional semantics.

newtype MapStore (name :: Symbol) = 
    MapStore (TVar (Map BS.ByteString BS.ByteString))

newtype MultiMapStore (name :: Symbol) = 
    MultiMapStore (TVar (Map BS.ByteString (Set BS.ByteString)))

data InMemoryStorage = InMemoryStorage

instance WithTx InMemoryStorage where    
    data Tx InMemoryStorage (m :: TxMode) = Whatever

    readOnlyTx _ f = f Whatever
    readWriteTx _ f = f Whatever


-- | Basic storage implemented using LMDB
instance Storage InMemoryStorage where    
    type SMapImpl InMemoryStorage = MapStore
    type SMultiMapImpl InMemoryStorage = MultiMapStore

    put _ (MapStore m) (SKey (Storable ks)) (SValue (Storable bs)) = 
        atomically $ modifyTVar' m $ Map.insert ks bs

    delete _ (MapStore m) (SKey (Storable ks)) = 
        atomically $ modifyTVar' m $ Map.delete ks

    get _ (MapStore m) (SKey (Storable ks)) =
        (SValue . Storable <$>) . Map.lookup ks <$> readTVarIO m        

    foldS _ (MapStore m) f a0 = do        
        mm <- readTVarIO m 
        z <- newIORef a0
        for_ (Map.toAscList mm) $ \(k, v) ->
            updateRef z $ \a -> 
                f a (SKey $ Storable k) (SValue $ Storable v)            
        readIORef z

    putMu _ (MultiMapStore m) (SKey (Storable ks)) (SValue (Storable vs)) = 
        atomically $ modifyTVar' m (Map.alter alterIt ks) 
            where 
                alterIt Nothing = Just $ Set.singleton vs
                alterIt (Just s) = Just $ Set.insert vs s

    deleteMu _ (MultiMapStore m) (SKey (Storable ks)) (SValue (Storable vs)) =
        atomically $ modifyTVar' m $ Map.alter alterIt ks            
            where 
                alterIt Nothing = Nothing
                alterIt (Just s) = let
                    s' = Set.delete vs s
                    in if Set.null s'
                        then Nothing 
                        else Just s'

    deleteAllMu _ (MultiMapStore m) (SKey (Storable ks)) = 
        atomically $ modifyTVar' m $ Map.delete ks

    foldMuForKey _ (MultiMapStore m) key@(SKey (Storable ks)) f a0 = do 
        mm <- readTVarIO m 
        let s = fromMaybe Set.empty $ Map.lookup ks mm        
        z <- newIORef a0
        for_ s $ \v -> 
            updateRef z $ \a -> 
                f a key (SValue $ Storable v)            
        readIORef z

    foldMu _ (MultiMapStore m) f a0 = do        
        mm <- readTVarIO m 
        z <- newIORef a0
        for_ (Map.toAscList mm) $ \(k, s) ->
            for_ s $ \v -> 
                updateRef z $ \a -> 
                    f a (SKey $ Storable k) (SValue $ Storable v)
        readIORef z


updateRef :: IORef a -> (a -> IO a) -> IO ()
updateRef z f = do 
    a <- readIORef z
    !a' <- f a
    writeIORef z a'


createMapStore :: forall name . KnownSymbol name => IO (MapStore name)
createMapStore = MapStore <$> newTVarIO Map.empty    

createMultiMapStore :: forall name . KnownSymbol name => IO (MultiMapStore name)
createMultiMapStore = MultiMapStore <$> newTVarIO Map.empty    

