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

module  SimRingCallApp where

import           Beseder.Base.ControlData
import           Beseder.Base.Common
import           Beseder.ARI.ARIHelper 
import           Beseder.ARI.ARICallProv 
import           Beseder.Resources.Timer
import           Data.String
import           GHC.Exts (Any)    
import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until, try, on)

simRingCallApp :: MakeCall -> MakeCall -> MakeCall -> Int -> Int -> STransData m NoSplitter _ () 
simRingCallApp orgMakeCallReq destMakeCallReq1 destMakeCallReq2 timeoutSec conTimeout = do
  withConnectedCall #call1 #t1 orgMakeCallReq timeoutSec $ do
    invoke #call1 PlayRingback
    simRingCall #call2 #call3 #tOut destMakeCallReq1 destMakeCallReq2 timeoutSec 
    try @("call2" :? IsCallAlive) $ do
      invoke #call1 StopRingback
      bridgeTwoCalls #call1 #call2 #bridge
      label #connected
      delay #tCon conTimeout 

mkSTransDataTypeAny "simRingCallApp" "SimRingCallApp"

type SimRingCallAppVal = ValidateSteps '["connected"] SimRingCallApp NoSplitter '[()]
type SimRingCallAppRes = Eval (SimRingCallApp NoSplitter '[()])
-- :kind! SimRingCallAppRes
-- :kind! SimRingCallAppVal 

  
  