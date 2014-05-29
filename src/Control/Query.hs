{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Query
-- Copyright   :  (c) 2014 Stefan Bühler
-- License     :  MIT-style (see the file COPYING)
--
-- Maintainer  :  stbuehler@web.de
-- Stability   :  experimental
-- Portability :  portable
--
-- Request/Response framework, handling queries in a separate thread,
-- should kill the thread if reference to server object gets lost.
--
-----------------------------------------------------------------------------

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

{-| internal request data -}
newtype Request a b = Request (a, MVar b)

{-|
Server object; the server (thread) gets killed if this object dies. Used to
send queries to the server thread.
-}
data QueryServer a b = QueryServer
	{ qsThread :: ThreadId
	, qsQuery :: MVar (Request a b)
	}

{-|
Type alias for a function handling requests in a 'QueryServer'. The first
parameter is the request data, the second the action to return the response
with.

Example:

> forkingHandler :: (a -> IO b) -> QueryHandler a b
> forkingHandler h req resp = forkIO $ h req >>= resp
-}
type QueryHandler a b = a -> (b -> IO()) -> IO ()

{-|
Start a new 'QueryServer' with the given handler
-}
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

{-|
Manually stop the 'QueryServer' instead of waiting for garbage collection.
-}
stopQueryServer :: QueryServer a b -> IO ()
stopQueryServer qs = throwTo (qsThread qs) ThreadKilled

{-|
Send a query to the 'QueryServer' and wait for the response.
-}
query :: forall a b. QueryServer a b -> a -> IO b
query qs req = do
	-- keep reference to QueryServer alive
	bracket (newStablePtr qs) freeStablePtr $ \_ -> do
		resp <- newEmptyMVar :: IO (MVar b)
		putMVar (qsQuery qs) $ Request (req, resp)
		takeMVar resp
