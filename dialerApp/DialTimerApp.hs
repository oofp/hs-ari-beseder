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

module  DialTimerApp where

import           Beseder.Base.ControlData
import           Beseder.Base.Common
import           Beseder.ARI.ARICallProv 
import           Beseder.ARI.ARICallProvImpl 
import           Beseder.Resources.Timer
import           Data.String
import           GHC.Exts (Any)    
import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until, try, on)

timerAppArr1 :: MakeCall -> Int -> STransData m NoSplitter _ () 
timerAppArr1 makeCallReq timeoutSec1 = do
  startTimer #timer timeoutSec1
  newRes #call ariCallRes

  try @("call1" :? IsCallAlive) $ do
    invoke #timer (StartTimer timeoutSec1)
    try @("timer" :? IsTimerArmed) $ do
      invoke #call  makeCallReq
      nextEv
      nextEv -- reach Connected
    on @("timer" :? IsTimerArmed) $ do
      invoke #timer StopTimer
      newRes #t TimerRes
      invoke #call PlayMusic -- Ringback
      invoke #t (StartTimer 5)
      nextEv
      invoke #call StopMusic 
    invoke #call DropCall
  termAndClearAllResources  
 
mkSTransDataTypeAny "timerAppArr1" "TimerAppArr1"

type TimerAppArr1Val = ValidateSteps '[] TimerAppArr1 NoSplitter '[()]
type TimerAppArr1Res = Eval (TimerAppArr1 NoSplitter '[()])
-- :kind! TimerAppArr1Res
-- :kind! TimerAppArr1Val 

--initCallServer :: ARIConfig -> Int -> IO ARIEnv
--initCallServer cfg timeoutSec = initCallServerData cfg (answerIncomingCallApp timeoutSec)
