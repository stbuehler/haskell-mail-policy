{-# LANGUAGE ScopedTypeVariables #-}

module Control.Query
	( QueryHandler
	, QueryServer
	, startQueryServer
	, stopQueryServer
	, query
	) where

import Foreign.StablePtr

import Control.Concurrent
import Control.Exception
import System.Mem.Weak (addFinalizer)

newtype Request a b = Request (a, MVar b)

data QueryServer a b = QueryServer
	{ qsThread :: ThreadId
	, qsQuery :: MVar (Request a b)
	}

type QueryHandler a b = a -> (b -> IO()) -> IO ()

startQueryServer :: forall a b. QueryHandler a b -> IO (QueryServer a b)
startQueryServer handler = do
		q <- newEmptyMVar :: IO (MVar (Request a b))
		t <- forkIO $ serve q
		let qs = QueryServer t q
		addFinalizer qs (throwTo t ThreadKilled)
		return qs
	where
	serve :: MVar (Request a b) -> IO ()
	serve input = do
		Request (req, resp) <- takeMVar input
		handler req (putMVar resp)
		serve input

stopQueryServer :: QueryServer a b -> IO ()
stopQueryServer qs = throwTo (qsThread qs) ThreadKilled

query :: forall a b. QueryServer a b -> a -> IO b
query qs req = do
	-- keep reference to QueryServer alive
	bracket (newStablePtr qs) freeStablePtr $ \_ -> do
		resp <- newEmptyMVar :: IO (MVar b)
		putMVar (qsQuery qs) $ Request (req, resp)
		takeMVar resp
