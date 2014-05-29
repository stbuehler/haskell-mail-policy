{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Policy.Types
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

module Network.Policy.Types
	( module Network.Policy.Action
	, PolicyParameters
	, PolicyHandler
	) where

import Network.Policy.Action

import qualified Data.Text as T
import qualified Data.Map.Strict as M

{-|
Type alias for parameters in a policy request (mapping keys to values).
-}
type PolicyParameters = M.Map T.Text T.Text

{-|
Type alias for functions handling a policy request
-}
type PolicyHandler = PolicyParameters -> IO PolicyAction
