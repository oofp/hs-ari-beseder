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
{-# OPTIONS_GHC -freduction-depth=2000 #-}

module  SimRingCallApp2 where

import           Beseder.Base.ControlData
import           Beseder.Base.Common
import           Beseder.ARI.ARIHelper 
--import           Beseder.ARI.ARIRunner
import           Beseder.ARI.ARICallProv 
import           Beseder.Resources.Timer
import           Data.String
import           GHC.Exts (Any)    
import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until, try, on)

simRingCallApp2 :: MakeCall -> MakeCall -> MakeCall -> Int -> Int -> STransData m NoSplitter _ () 
simRingCallApp2 orgMakeCallReq destMakeCallReq1 destMakeCallReq2 timeoutSec conTimeout = do
  withConnectedCall #call1 #t1 orgMakeCallReq timeoutSec $ do
    invoke #call1 PlayRingback
    simRingCall2 #call2 #call3 #tOut destMakeCallReq1 destMakeCallReq2 timeoutSec 
    try @("call2" :? IsCallAlive) $ do
      invoke #call1 StopRingback
      bridgeTwoCalls #call1 #call2 #bridge
      label #connected2
      delay #tCon conTimeout 
    try @("call3" :? IsCallAlive) $ do
      invoke #call1 StopRingback
      bridgeTwoCalls #call1 #call3 #bridge
      label #connected3
      delay #tCon conTimeout 

mkSTransDataTypeAny "simRingCallApp2" "SimRingCallApp2"

type SimRingCallApp2Val = ValidateSteps '["connected2","connected3"] SimRingCallApp2 NoSplitter '[()]
type SimRingCallApp2Res = Eval (SimRingCallApp2 NoSplitter '[()])
-- :kind! SimRingCallApp2Res
-- :kind! SimRingCallApp2Val 

  
--simRingCallApp2M orgMakeCallReq destMakeCallReq1 destMakeCallReq2 timeoutSec conTimeout = 
--  ariTransFromData (simRingCallApp2 orgMakeCallReq destMakeCallReq1 destMakeCallReq2 timeoutSec conTimeout) 
  
