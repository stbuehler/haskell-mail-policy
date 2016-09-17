{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Policy.Utils
-- Copyright   :  (c) 2014 Stefan Bühler
-- License     :  MIT-style (see the file COPYING)
--
-- Maintainer  :  stbuehler@web.de
-- Stability   :  experimental
-- Portability :  portable
--
-- Network and Monad related utility functions.
--
-----------------------------------------------------------------------------

module Network.Policy.Utils
	( makeListeningSocket
	, listenLocalhostIPv4
	, sockAddrFamily
	, setSocketNonBlocking
	, setSocketNonBlocking'
	, acceptLoop
	, acceptLoopFork
	, foldWhileLeftM
	) where

import Control.Concurrent (forkIO)
import Data.IP
import Foreign.C.Types
import Network.Socket
import System.Posix.IO
import System.Posix.Types (Fd(..))

{-|
Make socket non blocking (sadly this is not the default).
-}
setSocketNonBlocking :: Socket -> IO ()
setSocketNonBlocking sock = setSocketNonBlocking' sock True

{-|
Make socket non blocking or blocking depending on parameter.
-}
setSocketNonBlocking' :: Socket -> Bool -> IO ()
setSocketNonBlocking' sock onoff = setFdOption (Fd $ fdSocket sock) NonBlockingRead onoff

{-|
'Family' of a 'SockAddr' object (for creating a new 'Socket')
-}
sockAddrFamily :: SockAddr -> Family
sockAddrFamily (SockAddrInet _ _) = AF_INET
sockAddrFamily (SockAddrInet6 _ _ _ _) = AF_INET6
sockAddrFamily (SockAddrUnix _) = AF_UNIX

{-|
Create a listening socket from a file descriptor (fails if file descriptor
doesn't actually refer to a socket, but doesn't check whether it is actually
a 'Listening' socket).
-}
makeListeningSocket :: CInt -> IO Socket
makeListeningSocket fd = do
	-- create temporary 'Socket' of type AF_INET - any supported socket type
	-- is ok, we just need to read the family after getSocketName.
	-- garbage collection doesn't auto-close sockets, so this should be no problem
	tmps <- mkSocket fd AF_INET Stream defaultProtocol NotConnected
	tmpaddr <- getSocketName tmps
	-- now create real socket
	mkSocket fd (sockAddrFamily tmpaddr) Stream defaultProtocol Listening

{-|
Create a socket, bind it to the specified port on 127.0.0.1 and listen to it.
-}
listenLocalhostIPv4 :: PortNumber -> IO Socket
listenLocalhostIPv4 port = do
	s <- socket AF_INET Stream defaultProtocol
	setSocketOption s ReuseAddr 1
	bind s (SockAddrInet port $ toHostAddress $ "127.0.0.1")
	listen s maxListenQueue
	return s

{-|
Make listening socket non-blocking and run a accept-loop on it, handling
the accepted connections with the given handler.
-}
acceptLoop :: Socket -> (Socket -> SockAddr -> IO ()) -> IO ()
acceptLoop serv handle = do
		setSocketNonBlocking serv
		go
	where
	go = do
		(client, address) <- accept serv
		setSocketNonBlocking client
		handle client address
		go

{-|
Same as 'acceptLoop' but calls the handler in a separate thread.
-}
acceptLoopFork :: Socket -> (Socket -> SockAddr -> IO ()) -> IO ()
acceptLoopFork serv handle = acceptLoop serv $ \c a -> do
	_ <- forkIO $ handle c a
	return ()

{-|
Fold result of actions until the fold returns a Right instead of a Left (not
running the remaining actions) or all actions were run.
-}
foldWhileLeftM :: Monad m => (a -> b -> m (Either a a)) -> a -> [m b] -> m a
foldWhileLeftM _ acc [] = return acc
foldWhileLeftM f acc (x:xs) = x >>= f acc >>= \t -> case t of
	Left acc' -> foldWhileLeftM f acc' xs
	Right result -> return result
