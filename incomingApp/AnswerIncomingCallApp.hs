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
{-# LANGUAGE TemplateHaskell        #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module  AnswerIncomingCallApp where

import           Beseder.Base.ControlData
import           Beseder.Base.Common
import           Beseder.ARI.ARICallProv 
import           Beseder.Resources.Timer
import           Beseder.ARI.ARIRunner
import           Data.Text
import           Data.String
import           GHC.Exts (Any)    
import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until, try, on)


answerIncomingCallApp :: (_) => Int -> STransData m NoSplitter _ () 
answerIncomingCallApp timeout = do
  liftIO $ putStrLn ("***************** Entered answerIncomingCallApp"::Text)
  try @("call1" :? IsCallAlive) $ do
    liftIO $ putStrLn ("***************** answerIncomingCallApp: Going to accept call"::Text)
    invoke #call1 AcceptCall
    liftIO $ putStrLn ("***************** answerIncomingCallApp: call accepted"::Text)
    newRes #t0 TimerRes
    invoke #t0 (StartTimer 4)
    nextEv
    liftIO $ putStrLn ("***************** answerIncomingCallApp: Going to answer call"::Text)
    invoke #call1 AnswerCall
    nextEv
    invoke #call1 PlayMusic
    newRes #t1 TimerRes
    invoke #t1 (StartTimer timeout)
    nextEv
  on @("t0" :? IsTimerArmed) $ do   
    invoke #t0 StopTimer
  on @("t1" :? IsTimerArmed) $ do   
    invoke #t1 StopTimer
  on @("call1" :? IsCallConnected) $ do
    invoke #call1 DropCall
  clearAllResources  
 

mkSTransDataTypeAny "answerIncomingCallApp" "AnswerIncomingCallApp"

type AnswerIncomingCallAppVal = ValidateSteps '[] AnswerIncomingCallApp NoSplitter (OfferedCall ())
type AnswerIncomingCallAppRes = Eval (AnswerIncomingCallApp NoSplitter (OfferedCall ()))
-- :kind! AnswerIncomingCallAppRes
-- :kind! AnswerIncomingCallAppVal 

initCallServer :: ARIConfig -> Int -> IO ARIEnv
initCallServer cfg timeoutSec = initCallServerData cfg (answerIncomingCallApp timeoutSec)
