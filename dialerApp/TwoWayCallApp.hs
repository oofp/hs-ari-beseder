{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RebindableSyntax       #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell        #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module  TwoWayCallApp where

import           Beseder.Base.ControlData
import           Beseder.Base.Common
import           Beseder.ARI.ARIHelper 
import           Beseder.ARI.ARICallProv 
import           Beseder.Resources.Timer
import           Data.String
import           GHC.Exts (Any)    
import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until, try, on)

twoWayCallApp :: MakeCall -> MakeCall -> Int -> STransData m NoSplitter _ () 
twoWayCallApp makeCallReq1 makeCallReq2 timeoutSec = do
  withConnectedCall #call1 #t1 makeCallReq1 timeoutSec $ do
    withOutBoundCall #call2 makeCallReq2 $ do
      try @(Not ("call2" :? IsCallConnected)) $ do
        withTimer #t2 timeoutSec (invoke #call2 DropCall) $ do
          handleEvents $ do
            on @("call2" :? IsCallRingingBack) $ do
              invoke #call1 PlayRingback
      assert @("call2" :? IsCallConnected)
      on @("call1" :? IsCallPlayingRingback) (invoke #call1 StopRingback)
      bridgeTwoCalls #call1 #call2 #bridge
      label #bridgedCalls
      pumpEvents
  label #exiting
  --termAndClearAllResources
  
mkSTransDataTypeAny "twoWayCallApp" "TwoWayCallApp"

type TwoWayCallAppVal = ValidateSteps '["bridgedCalls","exiting"] TwoWayCallApp NoSplitter '[()]
type TwoWayCallAppRes = Eval (TwoWayCallApp NoSplitter '[()])
-- :kind! TwoWayCallAppRes
-- :kind! TwoWayCallAppVal 
