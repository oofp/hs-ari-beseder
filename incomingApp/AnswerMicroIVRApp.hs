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

module  AnswerMicroIVRApp where

import           Beseder.Base.ControlData
import           Beseder.Base.Common
import           Beseder.ARI.ARIMonad
import           Beseder.ARI.ARICallProv 
import           Beseder.ARI.ARIPlayerProv
import           Beseder.ARI.ARIPlayerProvImpl 
import           Beseder.Resources.Timer
import           Beseder.Resources.Task
import           ARICore    
import           Beseder.ARI.ARIRunner
import           Beseder.ARI.ARIData
import           Beseder.ARI.DTMFMonitorHelper 
import           Data.Text
import           Data.String
import           GHC.Exts (Any)    
import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until, try, on)

startMediaSession :: CallProv m pars => Named callName -> STransData m sp (ComposeFunc (InvokeAllFunc StartMediaSession callName) (BindFunc (OpResFunc callName (CallConnected m pars 'BridgeOff muteState 'MediaSession name2)) (ReturnFunc MediaHandle))) MediaHandle
startMediaSession callName = do                                                
  invoke callName StartMediaSession
  mediaHandle <- opRes callName getCallMediaHandle
  return mediaHandle

answerAndPlayApp :: (_) => Int -> STransData (AriM ()) NoSplitter _ () 
answerAndPlayApp timeout = do
  liftIO $ putStrLn ("***************** Entered answerAndPlayApp"::Text)
  try @("call1" :? IsCallAlive) $ do
    liftIO $ putStrLn ("***************** answerAndPlayApp: Going to accept call"::Text)
    invoke #call1 AcceptCall
    liftIO $ putStrLn ("***************** answerAndPlayApp: call accepted"::Text)
    newRes #t0 TimerRes
    invoke #t0 (StartTimer timeout)
    nextEv
    liftIO $ putStrLn ("***************** answerAndPlayApp: Going to answer call"::Text)
    invoke #call1 AnswerCall
    nextEv
    invoke #call1 StartMediaSession
    mediaHandle <- opRes #call1 getCallMediaHandle
    --repeatTimes 3 $ do
    do  
      newRes #player ariPlayerRes -- ARIPlayer
      invoke #player (PlayMedia (PBSound (PBSoundParams "hello-world")) mediaHandle)
      nextEv
      clear #player  
    firstDigitRes #dtmfMon mediaHandle
    try @("dtmfMon" :? IsTaskInProgress) $ do
      forever $ do
        label #playing1
        newRes #player ariPlayerRes
        invoke #player (PlayMedia (PBSound (PBSoundParams "for-tech-support")) mediaHandle)
        nextEv
        clear #player  
        label #playing2
  termAndClearAllResources 
  {-
  on @(By "dtmfMon")
    (clear #dtmfMon)
  on @(By "player")
    (clear #player)
  on @(By "t0")
    (clear #t0)
  on @(By "call1")
    (clear #call1)
  liftIO $ putStrLn ("***************** answerAndPlayApp: exiting"::Text)
-}

mkSTransDataTypeAny "answerAndPlayApp" "AnswerAndPlayApp"

type AnswerAndPlayAppVal = ValidateSteps '["playing1", "playing2"] AnswerAndPlayApp NoSplitter (OfferedCall ())
type AnswerAndPlayAppRes = Eval (AnswerAndPlayApp NoSplitter (OfferedCall ()))
-- :kind! AnswerAndPlayAppRes
-- :kind! AnswerAndPlayAppVal 

--tryToComp :: STrans (ContT Bool) (AriM ()) NoSplitter (OfferedCall ()) _ _ _ ()
--tryToComp  = interpret (answerAndPlayApp 1)
initCallServer :: ARIConfig -> Int -> IO ARIEnv
initCallServer cfg timeoutSec = 
  let app :: STransData (AriM ()) NoSplitter _ ()
      app = answerAndPlayApp timeoutSec
  in initCallServerData cfg app
