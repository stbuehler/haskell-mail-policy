{-# LANGUAGE OverloadedStrings #-}

module Network.Policy.Server
	( handleServerConnection
	) where

import Network.Policy.Types
import Network.Policy.Parser

import Control.Exception
import Control.Monad.State
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import qualified Data.Attoparsec as A
import qualified Data.ByteString as B
import System.Log.Logger
import Data.Text.Encoding (encodeUtf8)

handleServerConnection :: Logger -> PolicyHandler -> Socket -> IO ()
handleServerConnection logger handler sock = wrap $ evalStateT waitForData B.empty
	where
	waitForData :: StateT B.ByteString IO ()
	waitForData = do
		d <- get
		if B.null d
			then do
				d' <- readInput
				if B.null d'
					then return ()
					else readRequest
			else readRequest

	readInput :: StateT B.ByteString IO B.ByteString
	readInput = do
		d <- lift $ recv sock 4096
		modify (\old -> if B.null old then d else B.append old d)
		return d

	readRequest :: StateT B.ByteString IO ()
	readRequest = do
		d <- get
		result <- A.parseWith readInput parseRequest d
		case result of
			A.Done remainder res -> do
				total <- get
				let raw = B.take (B.length total - B.length remainder) total
				put remainder
				lift $ handleRequest $ PolicyRequest raw res
				waitForData
			A.Fail _ _ msg -> error $ "Couldn't parse request: " ++ msg
			A.Partial _ -> lift $ logL logger NOTICE ("Unexpected connection close")

	handleRequest :: PolicyRequest -> IO ()
	handleRequest req = do
		res <- handler req
		msg <- policyResultMessage res
		sendAll sock $ encodeUtf8 msg

	wrap :: IO () -> IO ()
	wrap act = finally (catch act handleErrorCall) (close sock)

	handleErrorCall :: ErrorCall -> IO ()
	handleErrorCall e = do
		logL logger ERROR $ show e
		return ()
