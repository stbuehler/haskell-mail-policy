{-# LANGUAGE OverloadedStrings #-}

module Network.Policy.Utils
	( makeListeningSocket
	, listenLocalhostIPv4
	, sockAddrFamily
	, setSocketNonBlocking
	, setSocketNonBlocking'
	, acceptLoop
	, acceptLoopFork
	) where

import Control.Concurrent (forkIO)
import Data.IP
import Foreign.C.Types
import Network.Socket
import System.Posix.IO
import System.Posix.Types (Fd(..))

setSocketNonBlocking :: Socket -> IO ()
setSocketNonBlocking sock = setSocketNonBlocking' sock True

setSocketNonBlocking' :: Socket -> Bool -> IO ()
setSocketNonBlocking' sock onoff = setFdOption (Fd $ fdSocket sock) NonBlockingRead onoff

sockAddrFamily :: SockAddr -> Family
sockAddrFamily (SockAddrInet _ _) = AF_INET
sockAddrFamily (SockAddrInet6 _ _ _ _) = AF_INET6
sockAddrFamily (SockAddrUnix _) = AF_UNIX

makeListeningSocket :: CInt -> IO Socket --  CInt -> Family -> SocketType -> ProtocolNumber -> SocketStatus -> IO Socket
makeListeningSocket fd = do
	tmps <- mkSocket fd AF_INET Stream defaultProtocol NotConnected
	tmpaddr <- getSocketName tmps
	mkSocket fd (sockAddrFamily tmpaddr) Stream defaultProtocol Listening

listenLocalhostIPv4 :: PortNumber -> IO Socket
listenLocalhostIPv4 port = do
	s <- socket AF_INET Stream defaultProtocol
	setSocketOption s ReuseAddr 1
	bind s (SockAddrInet port $ toHostAddress $ "127.0.0.1")
	listen s maxListenQueue
	return s

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

acceptLoopFork :: Socket -> (Socket -> SockAddr -> IO ()) -> IO ()
acceptLoopFork serv handle = acceptLoop serv $ \c a -> do
	_ <- forkIO $ handle c a
	return ()
