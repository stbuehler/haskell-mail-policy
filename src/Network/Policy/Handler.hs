{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, UndecidableInstances, RankNTypes #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Policy.Handler
-- Copyright   :  (c) 2014 Stefan Bühler
-- License     :  MIT-style (see the file COPYING)
--
-- Maintainer  :  stbuehler@web.de
-- Stability   :  experimental
-- Portability :  portable
--
-- More types for Network.Policy.
--
-----------------------------------------------------------------------------

module Network.Policy.Handler
	( module Network.Policy.Action
	, PolicyParameters
	, PolicyHandlerT
	, runPolicyHandler
	, evalPolicyHandler
	, getParameters
	, withParameter
	, withParameter'
	, PolicyHandler
	, handleWithFirstNonPass
	) where

import Network.Policy.Action
import Network.Policy.Utils

import qualified Data.Text as T
import qualified Data.Map.Strict as M

import Control.Monad.State.Strict
import Control.Applicative
import Control.Monad.Error

{-|
Type alias for parameters in a policy request (mapping keys to values).
-}
type PolicyParameters = M.Map T.Text T.Text

newtype PolicyHandlerT m a = PolicyHandlerT (StateT PolicyParameters m a)
	deriving (Functor, Applicative, Monad, MonadTrans, MonadFix, MonadPlus, MonadIO)

deriving instance MonadError e m => MonadError e (PolicyHandlerT m)

runPolicyHandler :: Monad m => PolicyHandlerT m a -> PolicyParameters -> m (a, PolicyParameters)
runPolicyHandler (PolicyHandlerT f) params = runStateT f params

evalPolicyHandler :: Monad m => PolicyHandlerT m a -> PolicyParameters -> m a
evalPolicyHandler (PolicyHandlerT f) params = evalStateT f params

getParameters :: Monad m => PolicyHandlerT m PolicyParameters
getParameters = PolicyHandlerT get

withParameter :: Monad m => T.Text -> (T.Text -> PolicyHandlerT m a) -> PolicyHandlerT m a -> PolicyHandlerT m a
withParameter key found notfound = do
	params <- getParameters
	case M.lookup key params of
		Just value -> found value
		Nothing -> notfound

withParameter' :: Monad m => T.Text -> (T.Text -> PolicyHandlerT m PolicyAction) -> PolicyHandlerT m PolicyAction
withParameter' key found = withParameter key found (return Policy_Pass)

type PolicyHandlerIO a = (forall m. MonadIO m => PolicyHandlerT m a)

{-|
Type alias for functions handling a policy request
-}
type PolicyHandler = PolicyHandlerIO PolicyAction


{-|
Run a list of policy handlers, stopping at the first not returning Policy_Pass
and returning its result (or returning Policy_Pass if all handlers were run).
-}
handleWithFirstNonPass :: MonadIO m => [PolicyHandlerT m PolicyAction] -> PolicyHandlerT m PolicyAction
handleWithFirstNonPass = foldWhileLeftM (\_ r -> case r of
		Policy_Pass -> return $ Left Policy_Pass
		_ -> return $ Right r
	) Policy_Pass
