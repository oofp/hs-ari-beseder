{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}  
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE OverloadedLabels       #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Beseder.ARI.ARIRunner 
    ( runAriTrans
    , initCallServer 
    , initCallServer'
    , rejectIncoming
    , runWithServerOnce
    , runAriTransData
    , runWithServerDataOnce
    , initCallServerData
    , ariTransFromData
    , IncomingTrans
    , IncomingTrans'
    , ARIDataFlow
    , OfferedCall
    , ARIConfig (..)
    , ARIEnv
    ) where

import ARICore    
import Beseder.ARI.ARIMonad
import Control.Monad.Cont
import Beseder.Base.Control hiding (return)
import Beseder.ARI.ARICallProv
import Beseder.ARI.ARICallProvImpl
import Protolude

emptyHandler :: Monad m => ARIEvent -> ChannelHandle -> m Bool
emptyHandler _ _ = return True

type OfferedCall s = '[CallOffered (AriM s) ARICall "call1"]

type IncomingTrans' s res f = STrans (ContT Bool) (AriM s) NoSplitter  (OfferedCall s) res '[] f () 
type IncomingTrans s f = IncomingTrans' s '[()] f

initCallServer' :: 
  ARIConfig  
  -> IncomingTrans s f
  -> s -> IO ARIEnv
initCallServer' cfg incomingCallHandler initialState = 
  let httpEnv = mkHTTPEnvFromConfig cfg
      flowSum channelHandle =
        let offeredCallTrans :: STrans (ContT Bool) (AriM s) NoSplitter '[()] _ _ _ ()
            offeredCallTrans = newRes #call1 (IncomingCallRes ARICall channelHandle)
            callTrans = (Beseder.Base.Control.>>) offeredCallTrans incomingCallHandler
            sumCont = execTrans  callTrans
        in runContT sumCont (\_->return False)
      incomingChannelHandler = createIncomingChannelHandler httpEnv emptyHandler flowSum (initStateWithDistr initialState)
  in initARI cfg incomingChannelHandler

initCallServer :: 
  ARIConfig  
  -> IncomingTrans () f
  -> IO ARIEnv
initCallServer cfg incomingCallHandler = initCallServer' cfg incomingCallHandler () 

rejectIncoming :: IncomingTrans s _ 
rejectIncoming = (Beseder.Base.Control.>>) (invoke #call1 DropCall) (clear #call1)

type ARICompletrTrans f = STrans (ContT Bool) (AriM ()) NoSplitter '[()] '[()] '[] f ()

runAriTrans :: ARIEnv -> ARICompletrTrans f -> IO (Either FlowError ())    
runAriTrans ariEnv ariTrans = 
  let appFlow =  execTrans ariTrans
      ariM = runContT appFlow (\_-> return False)
  in runAriM' ariEnv ariM    
  
runWithServerOnce :: ARIConfig
  -> ARICompletrTrans f
  -> IO (Either FlowError ())
runWithServerOnce cfg ariTrans = do 
  ariEnv <- initCallServer cfg rejectIncoming
  runAriTrans ariEnv ariTrans    

type ARIDataFlow f = STransData (AriM ()) NoSplitter f () 

ariTransFromData :: (Interpretable (ContT Bool) (AriM ()) NoSplitter '[()] rs '[] f) => ARIDataFlow f -> STrans (ContT Bool) (AriM ()) NoSplitter '[()] rs '[] f ()
ariTransFromData = interpret

completeARITransFromData :: (Interpretable (ContT Bool) (AriM ()) NoSplitter '[()] '[()] '[] f) => ARIDataFlow f -> ARICompletrTrans f
completeARITransFromData = interpret

incomingARITransFromData :: (Interpretable (ContT Bool) (AriM ()) NoSplitter (OfferedCall ()) '[()] '[] f) => ARIDataFlow f -> IncomingTrans () f
incomingARITransFromData = interpret

runAriTransData :: 
  ( Interpretable (ContT Bool) (AriM ()) NoSplitter '[()] '[()] '[] f
  ) => ARIEnv -> ARIDataFlow f -> IO (Either FlowError ())    
runAriTransData ariEnv ariTransData = 
  runAriTrans ariEnv (completeARITransFromData ariTransData)
  

runWithServerDataOnce :: 
  ( Interpretable (ContT Bool) (AriM ()) NoSplitter '[()] '[()] '[] f
  ) => ARIConfig
  -> ARIDataFlow f
  -> IO (Either FlowError ())
runWithServerDataOnce cfg ariTransData = 
  runWithServerOnce cfg (completeARITransFromData ariTransData)

initCallServerData :: (Interpretable (ContT Bool) (AriM ()) NoSplitter (OfferedCall ()) '[()] '[] f) => 
  ARIConfig  
  -> ARIDataFlow f
  -> IO ARIEnv
initCallServerData cfg incomingCallDataHandler = initCallServer cfg (incomingARITransFromData incomingCallDataHandler) 
