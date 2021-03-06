{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module RPKI.Parallel where

import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Reader
import           Numeric.Natural

import qualified Control.Concurrent.STM.TBQueue  as Q

import           Control.Exception.Lifted

import           Control.Concurrent.Async.Lifted
import           Control.Monad.Trans.Control

import           Data.List.NonEmpty              (NonEmpty (..))
import qualified Data.List.NonEmpty              as NonEmpty

import           RPKI.AppMonad
import           RPKI.Reporting
import           Streaming
import qualified Streaming.Prelude               as S

atLeastOne :: Natural -> Natural
atLeastOne n = if n < 2 then 1 else n

-- 
-- TODO Refactor it so that is shared code with "txFoldPipeline"

-- Consume a stream, map each element and put asyncs in the queue.
-- Read the queue and consume asyncs on the other end.
foldPipeline :: (MonadBaseControl IO m, MonadIO m) =>
            Bottleneck ->
            Stream (Of s) (ValidatorTCurried m) () ->
            (s -> ValidatorT m p) ->          -- ^ producer
            (p -> r -> ValidatorT m r) ->     -- ^ consumer, called for every item of the traversed argument
            r ->                   -- ^ fold initial value
            ValidatorT m r
foldPipeline bottleneck stream mapStream consume accum0 =
    snd <$> bracketChanClosable
                (atLeastOne $ maxBottleneckSize bottleneck)
                writeAll 
                readAll 
                cancel
    where        
        writeAll queue = S.mapM_ toQueue stream
            where 
                toQueue s = do
                    t <- async $ mapStream s
                    liftIO $ atomically $ writeCQueue queue t

        readAll queue = go accum0
            where
                go accum = do                
                    t <- liftIO $ atomically $ readCQueue queue
                    case t of
                        Nothing -> pure accum
                        Just t' -> do 
                            p <- wait t'
                            consume p accum >>= go

-- | Utility function for a specific case of producer-consumer pair 
-- where consumer works within a transaction (represented as withTx function)
--  
txFoldPipeline :: (MonadBaseControl IO m, MonadIO m) =>
            Natural ->
            Stream (Of q) (ValidatorTCurried m) () ->
            ((tx -> ValidatorT m r) -> ValidatorT m r) ->     -- ^ transaction in which all consumerers are wrapped
            (tx -> q -> r -> ValidatorT m r) ->    -- ^ consumer, called for every item of the traversed argument
            r ->                        -- ^ fold initial value
            ValidatorT m r
txFoldPipeline poolSize stream withTx consume accum0 =
    snd <$> bracketChanClosable
                (atLeastOne poolSize)
                writeAll 
                readAll 
                (\_ -> pure ())
  where
    writeAll queue = 
        S.mapM_ 
            (liftIO . atomically . writeCQueue queue) 
            stream
        
    readAll queue = withTx $ \tx -> go tx accum0
      where
        go tx accum = do                
            a <- liftIO $ atomically $ readCQueue queue
            case a of
                Nothing -> pure accum
                Just a' -> consume tx a' accum >>= go tx


-- | The same as `txFoldPipeline` but transaction is divided into chunks.
--
txFoldPipelineChunked :: (MonadBaseControl IO m, MonadIO m) =>
            Natural ->                                     -- ^ Amount of queue element to be processed within one transaction
            Stream (Of q) (ValidatorTCurried m) () ->
            ((tx -> ValidatorT m r) -> ValidatorT m r) ->  -- ^ transaction in which all consumerers are wrapped
            Natural -> 
            (tx -> q -> r -> ValidatorT m r) ->    -- ^ consumer, called for every item of the traversed argument
            r ->                                   -- ^ fold initial value
            ValidatorT m r
txFoldPipelineChunked poolSize stream withTx chunkSize consume accum0 =
    snd <$> bracketChanClosable
                (atLeastOne poolSize)
                writeAll 
                readAll 
                (\_ -> pure ())
  where
    writeAll queue = S.mapM_ toQueue stream
      where 
        toQueue = liftIO . atomically . writeCQueue queue

    readAll queue = 
        go Nothing chunkSize accum0
      where
        go maybeTx leftToRead accum = do 
            n <- liftIO $ atomically $ readCQueue queue                            
            case n of
                Nothing      -> pure accum
                Just nextOne ->
                    case maybeTx of
                        Nothing -> do 
                            accum' <- withTx $ \tx -> work tx nextOne
                            go Nothing chunkSize accum' 
                        Just tx -> work tx nextOne
          where                    
            work tx element = do 
                accum' <- consume tx element accum
                case leftToRead of
                    0 -> pure accum'
                    _ -> go (Just tx) (leftToRead - 1) accum'


bracketChanClosableVT :: (MonadBaseControl IO m, MonadIO m) =>
                Natural ->
                (ClosableQueue t -> ValidatorT m b) ->
                (ClosableQueue t -> ValidatorT m c) ->
                (t -> ValidatorT m w) ->
                ValidatorT m (b, c)
bracketChanClosableVT size produce consume kill = 
    bracketChanClosableImpl size produce consume kill concurrentTasks
    
bracketChanClosable :: (MonadBaseControl IO m, MonadIO m) =>
                Natural ->
                (ClosableQueue t -> m b) ->
                (ClosableQueue t -> m c) ->
                (t -> m w) ->
                m (b, c)
bracketChanClosable size produce consume kill = 
    bracketChanClosableImpl size produce consume kill concurrently


-- 
-- | Created two threads and queue between then. Calls
-- 'produce' in one thread and 'consume' in the other thread,
-- 'kill' is used to kill an item in the queue in case
-- the whole thing is interrupted with an exception.
--
bracketChanClosableImpl :: (MonadBaseControl IO m, MonadIO m) =>
                Natural 
                -> (ClosableQueue t -> m b) 
                -> (ClosableQueue t -> m c) 
                -> (t -> m w) 
                -> (m b -> m c -> m (b, c)) 
                -> m (b, c)
bracketChanClosableImpl size produce consume kill concurrentRun = do        
    queue <- liftIO $ atomically $ newCQueue size
    let closeQ = liftIO $ atomically $ closeCQueue queue
    concurrentRun
            (produce queue `finally` closeQ) 
            (consume queue `finally` closeQ)
        `finally`
            killAll queue kill

data QState = QWorks | QClosed
    deriving (Show, Eq)

-- Simplest closeable queue  
data ClosableQueue a = ClosableQueue (TBQueue a) (TVar QState)    

newCQueueIO :: Natural -> IO (ClosableQueue a)
newCQueueIO = atomically . newCQueue

newCQueue :: Natural -> STM (ClosableQueue a)
newCQueue n = ClosableQueue <$> newTBQueue n <*> newTVar QWorks

writeCQueue :: ClosableQueue a -> a -> STM ()
writeCQueue (ClosableQueue q s) qe =
    readTVar s >>= \case 
        QClosed -> pure ()
        QWorks  -> Q.writeTBQueue q qe

-- | Read elements from the queue in chunks and apply the function to 
-- each chunk
readQueueChunked :: ClosableQueue a -> Natural -> ([a] -> IO ()) -> IO ()
readQueueChunked cq chunkSize f = go
  where     
    go = atomically (readChunk chunkSize cq) >>= \case             
            []    -> pure ()
            chunk -> f chunk >> go              

closeCQueue :: ClosableQueue a -> STM ()
closeCQueue (ClosableQueue _ s) = writeTVar s QClosed

isClosedCQueue :: ClosableQueue a -> STM Bool
isClosedCQueue (ClosableQueue _ s) = (QClosed ==) <$> readTVar s 


readChunk :: Natural -> ClosableQueue a -> STM [a]
readChunk 0 _ = pure []
readChunk leftToRead cq@(ClosableQueue q queueState) =
    Q.tryReadTBQueue q >>= \case     
        Just z -> (z : ) <$> readChunk (leftToRead - 1) cq
        Nothing -> 
            readTVar queueState >>= \case 
                QClosed -> pure []
                QWorks  -> retry


readCQueue :: ClosableQueue a -> STM (Maybe a)
readCQueue (ClosableQueue q queueState) =
    Q.tryReadTBQueue q >>= \case    
        Just z' -> pure $ Just z'
        Nothing -> 
            readTVar queueState >>= \case 
                QClosed -> pure Nothing
                QWorks  -> retry


-- | Simple straioghtforward implementation of a "thread pool".
-- 
newtype Bottleneck = Bottleneck (NonEmpty (TVar Natural, Natural))
    deriving newtype Semigroup

newBottleneck :: Natural -> STM Bottleneck
newBottleneck n = do 
    currentSize <- newTVar 0
    pure $ Bottleneck $ (currentSize, n) :| []

newBottleneckIO :: Natural -> IO Bottleneck
newBottleneckIO = atomically . newBottleneck

-- Who is going to execute a task when the bottleneck is busy
data BottleneckExecutor = Requestor | Submitter

maxBottleneckSize :: Bottleneck -> Natural
maxBottleneckSize (Bottleneck bottlenecks) = 
        atLeastOne $ minimum $ NonEmpty.map snd bottlenecks


takeSlot, releaseSlot :: Bottleneck -> STM ()
takeSlot  (Bottleneck bottlenecks) = 
    forM_ bottlenecks $ \(currentSize, maxSize) -> do 
        cs <- readTVar currentSize
        if cs < maxSize
            then modifyTVar' currentSize succ
            else retry

releaseSlot (Bottleneck bottlenecks) = 
    forM_ bottlenecks $ \(currentSize, _) -> 
        modifyTVar' currentSize pred
            -- if s == 0 then 0 else s - 1

someSpaceInBottleneck :: Bottleneck -> STM Bool
someSpaceInBottleneck (Bottleneck bottlenecks) =
    fmap and $ 
        forM bottlenecks $ \(currentSize, maxSize) -> do 
            cs <- readTVar currentSize
            pure $ cs < maxSize       

-- A task can be asyncronous, executed by the requestor (lazy)
-- and executed by the submitter (strict).
data Task m a
    = AsyncTask (Async (StM m (Either AppError a, ValidationState)))
    | RequestorTask (ValidatorT m a)


-- | If the bootleneck is full, `io` will be executed by the caller of `waitTask`
lazyTask :: (MonadBaseControl IO m, MonadIO m) => 
            ValidatorT m a -> Bottleneck -> ValidatorT m (Task m a)                       
lazyTask io bottleneck = newTask io bottleneck Requestor

-- | If the bottleneck is full, io will be execute by the thread that calls newTask.
strictTask :: (MonadBaseControl IO m, MonadIO m) => 
            ValidatorT m a -> Bottleneck -> ValidatorT m (Task m a)                       
strictTask io bottleneck = newTask io bottleneck Submitter

-- | If the bottleneck is full, io will be execute by the thread that calls newTask.
pureTask :: (MonadBaseControl IO m, MonadIO m) => 
            a -> Bottleneck -> ValidatorT m (Task m a)                       
pureTask a = strictTask (pure $! a)


-- General case
newTask :: (MonadBaseControl IO m, MonadIO m) => 
            ValidatorT m a 
            -> Bottleneck 
            -> BottleneckExecutor -> ValidatorT m (Task m a)
newTask io bn@(Bottleneck bottlenecks) execution = do     
    env <- ask    
    -- NOTE: We do not capture the state here, because the task created
    -- Will start from its own local state == mempty. After the execution,
    -- will merge this local thread state in the caller's state using 
    -- `embedValidatorT`.
    -- 
    join $ liftIO $ atomically $ do     
        hasSomeSpace <- someSpaceInBottleneck bn     
        if hasSomeSpace 
            then do 
                incSizes                
                pure $! appLift $ AsyncTask <$> asyncForTask env                       
            else pure $ 
                case execution of
                    Requestor -> pure $ RequestorTask io -- wrap it in RequestorTask                    
                    Submitter -> appLift $ submitterTask env
        where            
                    
            incSizes = forM_ bottlenecks $ \(currentSize, _) -> modifyTVar' currentSize succ
            decSizes = forM_ bottlenecks $ \(currentSize, _) -> modifyTVar' currentSize pred

            asyncForTask env = async $ 
                                    runValidatorT env io 
                                    `finally` 
                                    liftIO (atomically decSizes)

            -- TODO This is not very safe w.r.t. exceptions.
            -- submitterTask :: ValidatorT m (Task m a)
            submitterTask env = do 
                liftIO $ atomically incSizes
                a <- asyncForTask env 
                -- Wait for either the task to finish, or until there's 
                -- some free space in the bottleneck.
                liftIO (atomically $ do 
                    spaceInBottlenecks <- someSpaceInBottleneck bn
                    unless spaceInBottlenecks $ void $ waitSTM a)
                    `onException`
                        cancel a

                pure $! AsyncTask a      



waitTask :: (MonadBaseControl IO m, MonadIO m) => Task m a -> ValidatorT m a
waitTask (RequestorTask t) = t
waitTask (AsyncTask a)     = embedValidatorT $ wait a

cancelTask :: (MonadBaseControl IO m, MonadIO m) => Task m a -> ValidatorT m ()
cancelTask (RequestorTask _) = pure ()
cancelTask (AsyncTask a)     = cancel a

concurrentTasks :: (MonadBaseControl IO m, MonadIO m) =>  
                    ValidatorT m a -> ValidatorT m b -> ValidatorT m (a, b)
concurrentTasks v1 v2 = do
    env <- askEnv
    embedValidatorT $ do 
        ((r1, vs1), (r2, vs2)) <- 
            concurrently 
                (runValidatorT env v1) 
                (runValidatorT env v2)        
        pure ((,) <$> r1 <*> r2, vs1 <> vs2)



data Slot = Free | Taken | DontTake

-- | 
inParallelVT :: (MonadBaseControl IO m, MonadIO m) =>
                Bottleneck 
                -> [a] 
                -> (a -> ValidatorT m b) 
                -> ValidatorT m [b]
inParallelVT bottleneck as f = do 
    env <- askEnv
    let size = maxBottleneckSize bottleneck
    queue <- liftIO $ atomically $ newCQueue size    
    snd <$> concurrently
                (writeAll env queue `finally` closeQ queue) 
                (readAll queue `finally` closeQ queue)
            `finally`
                killAll queue cancel        
    where                
        closeQ q = liftIO $ atomically $ closeCQueue q
        writeAll env queue = appLift $ go as  
            where 
                go [] = pure ()
                go (s : ss) = do 
                    slot <- liftIO $ newTVarIO Free
            
                    -- It is important to create the async first and then try to acquire 
                    -- the slot in the bottleneck, otherwise there's potential for 
                    -- deadlock in the system.            
                    a <- async $ do                     
                        z <- runValidatorT env (f s)
                                `finally` 
                             liftIO (atomically $ parallelReleaseSlot bottleneck slot)
                        pure $! z

                    queueClosed <- 
                        liftIO (atomically $ parallePutToTheQueueWhenReady queue bottleneck slot a)
                            `onException` 
                        cancel a

                    unless queueClosed $ go ss                          
        
        readAll queue =
            liftIO (atomically $ readCQueue queue) >>= \case            
                Nothing -> pure []
                Just t  -> do                     
                    r <- appLift $ wait t
                    case r of 
                        (Left e, vs) -> do 
                            closeQ queue
                            embedState vs
                            appError e
                        (Right z, vs) -> do 
                            embedState vs
                            rest <- readAll queue 
                            pure $! z : rest


inParallel :: Bottleneck 
            -> [a] 
            -> (a -> IO b) 
            -> IO [b]
inParallel bottleneck as f = do     
    let size = maxBottleneckSize bottleneck
    queue <- atomically $ newCQueue size    
    snd <$> concurrently
                (writeAll queue `finally` closeQ queue) 
                (readAll queue `finally` closeQ queue)
            `finally`
                killAll queue cancel        
    where                
        closeQ q = liftIO $ atomically $ closeCQueue q
        writeAll queue = go as  
            where 
                go [] = pure ()
                go (s : ss) = do 
                    slot <- liftIO $ newTVarIO Free

                    a <- async $ do                     
                            z <- f s 
                                    `finally` 
                                liftIO (atomically $ parallelReleaseSlot bottleneck slot)
                            pure $! z

                    queueClosed <- 
                        atomically (parallePutToTheQueueWhenReady queue bottleneck slot a)
                            `onException` 
                        cancel a

                    unless queueClosed $ go ss                            
        
        readAll queue = 
            atomically (readCQueue queue) >>= \case             
                Nothing -> pure []
                Just t  -> do                     
                    z    <- wait t
                    rest <- readAll queue 
                    pure $! z : rest                    


parallePutToTheQueueWhenReady :: ClosableQueue a -> Bottleneck -> TVar Slot -> a -> STM Bool
parallePutToTheQueueWhenReady queue bottleneck slot t = do
    closed <- isClosedCQueue queue
    unless closed $ do 
        readTVar slot >>= \case
            Free -> do 
                writeTVar slot Taken
                takeSlot bottleneck
            _ -> pure ()                                    
        writeCQueue queue t
    pure closed   


parallelReleaseSlot :: Bottleneck -> TVar Slot -> STM ()
parallelReleaseSlot bottleneck slot = do 
    readTVar slot >>= \case                            
        Taken -> releaseSlot bottleneck
        _     -> writeTVar slot DontTake


killAll :: MonadIO m => ClosableQueue t -> (t -> m a) -> m ()
killAll queue kill = do
    a <- liftIO $ atomically $ readCQueue queue    
    case a of   
        Nothing -> pure ()
        Just as -> kill as >> killAll queue kill