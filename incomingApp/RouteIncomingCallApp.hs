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
{-# LANGUAGE OverloadedLabels       #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module  RouteIncomingCallApp where

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

routeIncomingCallApp :: (_) => Int -> MakeCall -> STransData m NoSplitter _ ()  
routeIncomingCallApp timeout makeCallReq = do
  liftIO $ putStrLn ("***************** Entered routeIncomingCallApp"::Text)
  try @("call1" :? IsCallAlive) $ do
    newRes #timer TimerRes
    invoke #timer (StartTimer timeout)
    try @(Not ("timer" :? IsTimerTriggered)) $ do
      newRes #call2 ariCallRes
      invoke #call2  makeCallReq 
      try @("call2" :? IsCallAlive) $ do
        try @(Not ("call2" :? IsCallConnected)) $ do
          handleEvents $ do
            on @("call2" :? IsCallRingingBack) $ do
              invoke #call1 AcceptCall
        invoke #call1 AnswerCall
        invoke #timer StopTimer
        nextEv
        newRes #bridge BridgeRes
        bridgeHandle <- gets #bridge getBridgeHandle
        noop
        invoke #call1 (AddToBridge bridgeHandle)
        invoke #call2 (AddToBridge bridgeHandle)
        nextEv -- call1 or call2 will be disconnected
  termAndClearAllResources        

mkSTransDataTypeAny "routeIncomingCallApp" "RouteIncomingCallApp"

type RouteIncomingCallAppVal = ValidateSteps '[] RouteIncomingCallApp NoSplitter (OfferedCall ())
type RouteIncomingCallAppRes = Eval (RouteIncomingCallApp NoSplitter (OfferedCall ()))
-- :kind! RouteIncomingCallAppRes
-- :kind! RouteIncomingCallAppVal 


initCallServer :: ARIConfig -> Int -> MakeCall -> IO ARIEnv
initCallServer cfg timeoutSec makeCallReq = initCallServerData cfg (routeIncomingCallApp timeoutSec makeCallReq)
