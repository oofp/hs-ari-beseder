{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE LambdaCase   #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Beseder.ARI.ARIBridge 
  ( BridgeRes (..)
  , getBridgeHandle
  ) where

import           Haskus.Utils.Variant
import           Beseder.Base.Base
import           Beseder.Base.Common
import           ARICore    
import           Beseder.ARI.ARIMonad (AriM)
import           Protolude hiding (TypeError)

data BridgeRes = BridgeRes deriving (Show, Eq)

instance CreateRes (AriM s) name BridgeRes (V '[St BridgeHandle name]) where
  createRes _named BridgeRes = 
    fmap (variantFromValue . St) createBridgeCmd  

instance (m ~ AriM s) => MkRes m BridgeRes  where
  type ResSt m BridgeRes  = BridgeHandle
  mkRes BridgeRes = createBridgeCmd

type instance StateTrans (St BridgeHandle name) = 'Static

instance (m ~ AriM s) => TermState m (St BridgeHandle name) where
  terminate (St bridgeHandle) = deleteBridgeCmd bridgeHandle

getBridgeHandle :: (St BridgeHandle name) -> BridgeHandle
getBridgeHandle (St h) = h 

type instance Eval (SupportedRequests (St BridgeHandle name)) =    '[]
type instance StateTitle BridgeHandle = "Bridge"
