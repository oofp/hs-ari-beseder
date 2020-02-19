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
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module  Beseder.ARI.ARIHelper 
  ( startCall 
  , withOutBoundCall
  , connectCall
  , withConnectedCall
  , bridgeTwoCalls
  , simRingCall
  , simRingCall2
  ) where

import           Beseder.Base.ControlData
import           Beseder.Base.Common
import           Beseder.ARI.ARICallProv 
import           Beseder.ARI.ARIBridge
import           Beseder.ARI.ARICallProvImpl 
import           Beseder.Resources.Timer
import           Data.String
import           GHC.Exts (Any)    
import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until, try, on)

startCall :: Named name -> MakeCall -> STransData m sp _ () 
startCall callName makeCallReq = do
  newRes callName ariCallRes
  invoke callName makeCallReq

startCall' = startCall #call1
mkSTransDataTypeAny "startCall'" "StartCall"

type StartCallVal = ValidateSteps '[] StartCall NoSplitter '[()]
type StartCallRes = Eval (StartCall NoSplitter '[()])

withOutBoundCall :: forall name m sp. (_) => Named name -> MakeCall -> STransData m (sp :&& name :? IsCallAlive) _ () -> STransData m sp _ () 
withOutBoundCall callName makeCallReq hnd = scopeRes $ do
  startCall callName makeCallReq
  try @(name :? IsCallAlive) $ hnd

connectCall' :: forall name m sp. Named name -> MakeCall -> STransData m sp _ () 
connectCall' callName makeCallReq = do
  startCall callName makeCallReq
  try @(name :? IsCallAlive) $ do
    skipTo @(name :? IsCallConnected) 

connectCall :: forall name m sp. Named name -> MakeCall -> STransData m sp _ () 
connectCall callName makeCallReq = do
  connectCall' callName makeCallReq
  on @(Not (name :? IsCallConnected)) $ clear callName

connectCall'' = connectCall #callName
mkSTransDataTypeAny "connectCall''" "ConnectCall"

type ConnectCallVal = ValidateSteps '[] ConnectCall NoSplitter '[()]
type ConnectCallRes = Eval (ConnectCall NoSplitter '[()])
-- :kind! TimerAppArr1Res
-- :kind! TimerAppArr1Val 

connectCallTimer :: forall c_name t_name m sp. Named c_name -> Named t_name -> MakeCall -> Int -> STransData m sp _ () 
connectCallTimer callName timerName makeCallReq connectTimeoutSec = do
  withTimeLimit timerName connectTimeoutSec 
                  (connectCall' callName  makeCallReq)
  on @(By c_name :&& (Not (c_name :? IsCallConnected))) (clear callName)
  label #conectCallDone

withConnectedCall :: forall c_name t_name m sp f_hnd. Named c_name -> 
  Named t_name -> MakeCall -> Int -> STransData m (sp :&& (c_name :? IsCallConnected)) f_hnd () -> STransData m sp _ () 
withConnectedCall callName timerName makeCallReq connectTimeoutSec hnd = scopeRes $ do
  connectCallTimer callName timerName makeCallReq connectTimeoutSec
  try @(c_name :? IsCallConnected) hnd

---
connectCallTimer' = connectCallTimer #callName #timerName
mkSTransDataTypeAny "connectCallTimer'" "ConnectCallTimer"

type ConnectCallTimerVal = ValidateSteps '["conectCallDone"] ConnectCallTimer NoSplitter '[()]
type ConnectCallTimerRes = Eval (ConnectCallTimer NoSplitter '[()])
-- :kind! ConnectCallTimerVal
-- :kind! ConnectCallTimerRes 

--
bridgeTwoCalls :: forall c1_name c2_name brd_name m sp. Named c1_name -> Named c2_name -> Named brd_name ->STransData m sp _ () 
bridgeTwoCalls call1Name call2Name bridgeName = do
  newRes bridgeName BridgeRes
  bridgeHandle <- gets bridgeName getBridgeHandle
  invoke call1Name (AddToBridge bridgeHandle)
  invoke call2Name (AddToBridge bridgeHandle)

--
simRingCall :: forall c1_name c2_name t_name m sp. Named c1_name -> Named c2_name -> Named t_name -> MakeCall -> MakeCall -> Int -> STransData m sp _ ()  
simRingCall call1Name call2Name  timerName makeCallReq1 makeCallReq2 timeoutSec = do
  startCall call1Name makeCallReq1
  startCall call2Name makeCallReq2
  try @(c1_name :? IsCallAlive :|| c2_name :? IsCallAlive) $ do
    skipWithTimeLimitTo @(c1_name :? IsCallConnected :|| c2_name :? IsCallConnected) timerName timeoutSec
  on @(Not (c1_name :? IsCallConnected)) $ clear call1Name    
  onOrElse @(c2_name :? IsCallConnected) 
    (renameRes call2Name call1Name)
    (clear call2Name)    
  label #simRingExit

simRingCall' = simRingCall #c1 #c2 #timer
mkSTransDataTypeAny "simRingCall'" "SimRingCall"

type SimRingCallVal = ValidateSteps '["simRingExit"] SimRingCall NoSplitter '[()]
type SimRingCallRes = Eval (SimRingCall NoSplitter '[()])
-- :kind! SimRingCallVal
-- :kind! SimRingCallRes 

--
simRingCall2 :: forall c1_name c2_name t_name m sp. Named c1_name -> Named c2_name -> Named t_name -> MakeCall -> MakeCall -> Int -> STransData m sp _ ()  
simRingCall2 call1Name call2Name  timerName makeCallReq1 makeCallReq2 timeoutSec = do
  startCall call1Name makeCallReq1
  startCall call2Name makeCallReq2
  try @(c1_name :? IsCallAlive :|| c2_name :? IsCallAlive) $ do
    skipWithTimeLimitTo @(c1_name :? IsCallConnected :|| c2_name :? IsCallConnected) timerName timeoutSec 
  block $ do  
    on @(Not (c1_name :? IsCallConnected)) $ clear call1Name    
    on @(Not (c2_name :? IsCallConnected)) $ clear call2Name    
  label #simRing2Exit

simRingCall2' = simRingCall2 #c1 #c2 #timer
mkSTransDataTypeAny "simRingCall2'" "SimRingCall2"

type SimRingCallVal2 = ValidateSteps '["simRing2Exit"] SimRingCall2 NoSplitter '[()]
type SimRingCallRes2 = Eval (SimRingCall2 NoSplitter '[()])
-- :kind! SimRingCallVal2
-- :kind! SimRingCallRes2 

