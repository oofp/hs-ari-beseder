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

module  RouteIncomingCallApp2 where

import           Beseder.Base.ControlData
import           Beseder.Base.Common
import           Beseder.ARI.ARICallProv 
import           Beseder.ARI.ARICallProvImpl 
import           Beseder.ARI.ARIBridge 
import           Beseder.Resources.Timer
import           Beseder.ARI.ARIRunner
import           Data.Text
import           Data.String
import           GHC.Exts (Any)    
import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until, try, on)

routeIncomingCallApp2 :: (_) => Int -> MakeCall -> STransData m NoSplitter _ ()  
routeIncomingCallApp2 timeout makeCallReq = do
  liftIO $ putStrLn ("***************** Entered routeIncomingCallApp"::Text)
  try @(Not ("call1" :? IsCallTerminated :|| "call2" :? IsCallTerminated :|| "timer" :? IsTimerTriggered)) $ do
    newRes #timer TimerRes
    invoke #timer (StartTimer timeout)
    newRes #call2 ariCallRes
    invoke #call2  makeCallReq
    try @(Not ("call2" :? IsCallRingingBack :|| "call2" :? IsCallConnected)) 
      pumpEvents
    on @("call2" :? IsCallRingingBack) $ do 
      invoke #call1 AcceptCall
      nextEv
    on @("call2" :? IsCallConnected) $ do --will be always connected 
      invoke #timer StopTimer
      invoke #call1 AnswerCall
      nextEv
    --connected now
    newRes #bridge BridgeRes
    bridgeHandle <- gets #bridge getBridgeHandle
    invoke #call1 (AddToBridge bridgeHandle)
    invoke #call2 (AddToBridge bridgeHandle)
        
    nextEv
  termAndClearAllResources
  liftIO $ putStrLn ("***************** Leavng routeIncomingCallApp"::Text)



mkSTransDataTypeAny "routeIncomingCallApp2" "RouteIncomingCallApp2"

type RouteIncomingCallAppVal2 = ValidateSteps '[] RouteIncomingCallApp2 NoSplitter (OfferedCall ())
type RouteIncomingCallAppRes2 = Eval (RouteIncomingCallApp2 NoSplitter (OfferedCall ()))
-- :kind! RouteIncomingCallAppRes2
-- :kind! RouteIncomingCallAppVal2 


initCallServer2 :: ARIConfig -> Int -> MakeCall -> IO ARIEnv
initCallServer2 cfg timeoutSec makeCallReq = initCallServerData cfg (routeIncomingCallApp2 timeoutSec makeCallReq)

