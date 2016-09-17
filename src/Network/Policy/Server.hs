{-# LANGUAGE OverloadedStrings, RankNTypes #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Policy.Server
-- Copyright   :  (c) 2014 Stefan Bühler
-- License     :  MIT-style (see the file COPYING)
--
-- Maintainer  :  stbuehler@web.de
-- Stability   :  experimental
-- Portability :  portable
--
-- Server implementation to handle policy requests with a custom handler.
--
-----------------------------------------------------------------------------

module Network.Policy.Server
	( handleServerConnection
	) where

import Network.Policy.Handler
import Network.Policy.Serialize

import Control.Exception
import Control.Monad.State
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import qualified Data.Attoparsec as A
import qualified Data.ByteString as B
import System.Log.Logger

{-|
Handle request on a connection with the given handler, logging errors to the
given logger.
-}
handleServerConnection :: Logger -> PolicyHandler -> Socket -> IO ()
handleServerConnection logger handler sock = wrap $ evalStateT waitForData B.empty
	where
	waitForData :: StateT B.ByteString IO ()
	waitForData = do
		d <- get
		if B.null d
			then do
				d' <- readInput
				put d'
				if B.null d'
					then return ()
					else readRequest
			else readRequest

	readInput :: StateT B.ByteString IO B.ByteString
	readInput = lift $ recv sock 4096

	readRequest :: StateT B.ByteString IO ()
	readRequest = do
		d <- get
		put B.empty
		result <- A.parseWith readInput parsePolicyParameters d
		case result of
			A.Done remainder res -> do
				put remainder
				lift $ handleRequest res
				waitForData
			A.Fail _ _ msg -> error $ "Couldn't parse request: " ++ msg
			A.Partial _ -> lift $ logL logger NOTICE ("Unexpected connection close")

	handleRequest :: PolicyParameters -> IO ()
	handleRequest req = do
		res <- evalPolicyHandler handler req
		msg <- formatPolicyAction res
		sendAll sock msg

	wrap :: IO () -> IO ()
	wrap act = finally (catch act handleErrorCall) (close sock)

	handleErrorCall :: ErrorCall -> IO ()
	handleErrorCall e = do
		logL logger ERROR $ show e
		return ()
