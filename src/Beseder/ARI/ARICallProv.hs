{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE LambdaCase   #-}

module Beseder.ARI.ARICallProv where

import           Haskus.Utils.Variant
import           Beseder.Base.Base
import           Beseder.Base.Common
import           Beseder.Base.Control
import           ARICore    
import           Beseder.ARI.ARIData
import           Protolude hiding (TypeError)
import           Haskus.Utils.Types.List
import           GHC.TypeLits

newtype CallRes pars = CallRes pars deriving Show
data IncomingCallRes pars = IncomingCallRes pars ChannelHandle deriving Show

data MakeCall = MakeCall 
  { tech :: Tech
  , prov :: Provider
  , callerID :: Party
  , dest :: Party
  } deriving (Show, Eq)

data DropCall = DropCall deriving (Show, Eq)
instance GetInstance DropCall where getInstance = DropCall

data MuteCall = MuteCall deriving (Show, Eq)    
data UnmuteCall = UnmuteCall deriving (Show, Eq)
newtype AddToBridge = AddToBridge BridgeHandle deriving (Show, Eq)
data RemoveFromBridge = RemoveFromBridge deriving (Show, Eq)
data PlayRingback = PlayRingback deriving (Show, Eq)
data StopRingback = StopRingback deriving (Show, Eq)
data PlayMusic = PlayMusic deriving (Show, Eq)
data StopMusic = StopMusic deriving (Show, Eq)
data AnswerCall = AnswerCall deriving (Show, Eq)
data AcceptCall = AcceptCall deriving (Show, Eq)
data StartMediaSession = StartMediaSession deriving (Show, Eq)
data StopMediaSession = StopMediaSession deriving (Show, Eq)

data MuteState = MuteOn | MuteOff deriving (Show, Eq)

data MediaState = NoMedia | PlayingRingback | PlayingMusic | MediaSession deriving (Show, Eq)

data BridgeState = BridgeOn | BridgeOff deriving (Show, Eq)

class CallProv (m :: * -> *) pars where
  data CallCreatedData m pars
  data CallOfferedData m pars
  data CallAcceptedData m pars
  data CallAnsweringData m pars
  data CallInitiatedData m pars
  data CallFailedData m pars
  data CallRingbackData m pars
  data CallConnectedData m pars (bridgeState :: BridgeState) (muteState :: MuteState) (mediaState :: MediaState)
  data CallDisconnectedData m pars

  createCall :: pars -> m (CallCreatedData m pars)
  createIncomingCall :: pars -> ChannelHandle -> m (CallOfferedData m pars)
  acceptCall :: AcceptCall -> CallOfferedData m pars -> m (V '[CallAcceptedData m pars, CallDisconnectedData m pars])  
  answerOfferedCall :: AnswerCall -> CallOfferedData m pars -> m (V '[CallAnsweringData m pars, CallDisconnectedData m pars])  
  answerAcceptedCall :: AnswerCall -> CallAcceptedData m pars -> m (V '[CallAnsweringData m pars, CallDisconnectedData m pars])  
  makeCall :: MakeCall -> CallCreatedData m pars -> m (V '[CallInitiatedData m pars, CallFailedData m pars])  
  dropInitiateCall :: DropCall -> CallInitiatedData m pars -> m (CallDisconnectedData m pars)
  dropRingbackCall :: DropCall -> CallRingbackData m pars -> m (CallDisconnectedData m pars)
  dropConnectedCall :: DropCall -> CallConnectedData m pars bridgeState muteState mediaState -> m (CallDisconnectedData m pars)
  dropOfferedCall :: DropCall -> CallOfferedData m pars -> m (CallDisconnectedData m pars)
  dropAcceptedCall :: DropCall -> CallAcceptedData m pars -> m (CallDisconnectedData m pars)
  dropAnsweringCall :: DropCall -> CallAnsweringData m pars -> m (CallDisconnectedData m pars)
  muteCall :: MuteCall -> CallConnectedData m pars bridgeState 'MuteOff mediaState -> m (CallConnectedData m pars bridgeState 'MuteOn mediaState)       
  unmuteCall :: UnmuteCall -> CallConnectedData m pars bridgeState 'MuteOn mediaState -> m (CallConnectedData m pars bridgeState 'MuteOff mediaState)     
  addToBridge :: AddToBridge -> CallConnectedData m pars 'BridgeOff muteState 'NoMedia -> m (CallConnectedData m pars 'BridgeOn muteState 'NoMedia)       
  removeFromBridge :: RemoveFromBridge -> CallConnectedData m pars 'BridgeOn muteState 'NoMedia -> m (CallConnectedData m pars 'BridgeOff muteState 'NoMedia)     
  playRingback :: PlayRingback -> CallConnectedData m pars 'BridgeOff muteState 'NoMedia -> m (CallConnectedData m pars 'BridgeOff muteState 'PlayingRingback)       
  stopRingback :: StopRingback -> CallConnectedData m pars 'BridgeOff muteState 'PlayingRingback -> m (CallConnectedData m pars 'BridgeOff muteState 'NoMedia)       
  playMusic :: PlayMusic -> CallConnectedData m pars 'BridgeOff muteState 'NoMedia -> m (CallConnectedData m pars 'BridgeOff muteState 'PlayingMusic)       
  stopMusic :: StopMusic -> CallConnectedData m pars 'BridgeOff muteState 'PlayingMusic -> m (CallConnectedData m pars 'BridgeOff muteState 'NoMedia)       
  startMediaSession :: StartMediaSession -> CallConnectedData m pars 'BridgeOff muteState 'NoMedia -> m (CallConnectedData m pars 'BridgeOff muteState 'MediaSession)        
  stopMediaSession :: StopMediaSession -> CallConnectedData m pars 'BridgeOff muteState 'MediaSession -> m (CallConnectedData m pars 'BridgeOff muteState 'NoMedia)        

  clearDisconnectedCall ::  CallDisconnectedData m pars -> m ()
  clearCreatedCall ::  CallCreatedData m pars -> m ()
  clearFailedCall ::  CallFailedData m pars -> m ()

  initiatedTransition :: CallTrans m (CallInitiatedData m pars) (InitiatedTransitionNextStates m pars)
  ringbackTransition ::  CallTrans m (CallRingbackData m pars) (RingbackTransitionNextStates m pars)
  connectedTransition ::  CallTrans m (CallConnectedData m pars bridgeState muteState mediaState) '[CallDisconnectedData m pars]
  offeredTransition ::  CallTrans m (CallOfferedData m pars) '[CallDisconnectedData m pars]
  answeringTransition ::  CallTrans m (CallAnsweringData m pars) (AnsweringTransitionNextStates m pars)
  acceptedTransition ::  CallTrans m (CallAcceptedData m pars) (AcceptedTransitionNextStates m pars)

  getChannelMediaHandle :: CallConnectedData m pars 'BridgeOff muteState 'MediaSession -> m MediaHandle

type InitiatedTransitionNextStates m pars = 
  [ CallRingbackData m pars
  , CallConnectedData m pars 'BridgeOff 'MuteOff 'NoMedia
  , CallDisconnectedData m pars
  ]   

type RingbackTransitionNextStates m pars = 
  [ CallConnectedData m pars 'BridgeOff 'MuteOff 'NoMedia
  , CallDisconnectedData m pars
  ]   

type OfferedTransitionNextStates m pars = 
 '[ CallDisconnectedData m pars
  ]   

type AcceptedTransitionNextStates m pars = 
 '[ CallDisconnectedData m pars
  ]   
  
type AnsweringTransitionNextStates m pars = 
  [ CallConnectedData m pars 'BridgeOff 'MuteOff 'NoMedia
  , CallDisconnectedData m pars
  ]   
     
type ConnectedTransitionNextStates m pars = 
  '[ CallDisconnectedData m pars
   ]   
    
type CallTrans m fromState toStates = fromState -> (V toStates -> m Bool) -> m Bool   

type CallCreated m pars name = St (CallCreatedData m pars) name
type CallInitiated m pars name = St (CallInitiatedData m pars) name
type CallFailed m pars name = St (CallFailedData m pars) name
type CallConnected m pars bridgeState muteState mediaState name = St (CallConnectedData m pars bridgeState muteState mediaState) name
type CallDisconnected m pars name = St (CallDisconnectedData m pars) name
type CallRingback m pars name = St (CallRingbackData m pars) name
type CallOffered m pars name = St (CallOfferedData m pars) name
type CallAccepted m pars name = St (CallAcceptedData m pars) name
type CallAnswering m pars name = St (CallAnsweringData m pars) name
                
type CallCompleted m pars name = By (CallDisconnected m pars name) :|| By (CallFailed m pars name)
type CallAlive m pars name = (By name) :&&  (Not (CallCompleted m pars name)) :&& (Not (By (St () name)))

{-
type CallAlive m pars name  
  =   By (CallInitiated m pars name) 
  :|| By (CallRingback m pars name) 
  :|| By (CallConnected m pars bridgeState muteState mediaState name) 
  :|| By (CallCreated m pars name) 

-}
type family IsCallConnectedFam a :: Bool where
  IsCallConnectedFam (CallConnected m pars bridgeState muteState mediaState name) = 'True
  IsCallConnectedFam _ = 'False
data IsCallConnected :: Type -> Exp Bool 
type instance Eval (IsCallConnected a) = IsCallConnectedFam a

type family IsCallRingingBackFam a :: Bool where
  IsCallRingingBackFam (CallRingback m pars name) = 'True
  IsCallRingingBackFam _ = 'False
data IsCallRingingBack :: Type -> Exp Bool 
type instance Eval (IsCallRingingBack a) = IsCallRingingBackFam a

type family IsCallPlayingRingbackFam a :: Bool where
  IsCallPlayingRingbackFam (CallConnected m pars _ _ 'PlayingRingback name) = 'True
  IsCallPlayingRingbackFam _ = 'False
data IsCallPlayingRingback :: Type -> Exp Bool 
type instance Eval (IsCallPlayingRingback a) = IsCallPlayingRingbackFam a

type family IsCallAliveFam a :: Bool where
  IsCallAliveFam (CallConnected m pars bridgeState muteState mediaState name) = 'True
  IsCallAliveFam (CallOffered m pars name) = 'True
  IsCallAliveFam (CallRingback m pars name) = 'True
  IsCallAliveFam (CallAccepted m pars name) = 'True
  IsCallAliveFam (CallAnswering m pars name) = 'True
  IsCallAliveFam (CallInitiated m pars name) = 'True
  IsCallAliveFam _ = 'False
data IsCallAlive :: Type -> Exp Bool 
type instance Eval (IsCallAlive a) = IsCallAliveFam a

type family IsCallTerminatedFam a :: Bool where
  IsCallTerminatedFam (CallFailed m pars name) = 'True
  IsCallTerminatedFam (CallDisconnected m pars name) = 'True
  IsCallTerminatedFam _ = 'False
data IsCallTerminated :: Type -> Exp Bool 
type instance Eval (IsCallTerminated a) = IsCallTerminatedFam a

type instance StateTrans (CallInitiated m pars name) = 'Dynamic
type instance StateTrans (CallRingback m pars name) = 'Dynamic
type instance StateTrans (CallConnected m pars bridgeState muteState mediaState name) = 'Dynamic
type instance StateTrans (CallOffered m pars name) = 'Dynamic
type instance StateTrans (CallAccepted m pars name) = 'Dynamic
type instance StateTrans (CallAnswering m pars name) = 'Dynamic
type instance StateTrans (CallCreated m pars name) = 'Static
type instance StateTrans (CallFailed m pars name) = 'Static
type instance StateTrans (CallDisconnected m pars name) = 'Static

type instance TermRequest (CallInitiated m pars name) = DropCall
type instance TermRequest (CallRingback m pars name) = DropCall
type instance TermRequest (CallOffered m pars name) = DropCall
type instance TermRequest (CallAnswering m pars name) = DropCall
type instance TermRequest (CallAccepted m pars name) = DropCall
type instance TermRequest (CallConnected m pars bridgeState muteState mediaState name) = DropCall

instance (CallProv m pars, MonadIO m) => TermState m (CallDisconnected m pars name) where
  terminate (St callDiscData) = clearDisconnectedCall callDiscData

instance (CallProv m pars, MonadIO m) => TermState m (CallFailed m pars name) where
  terminate (St callFailedData) = clearFailedCall callFailedData
  
instance (CallProv m pars, MonadIO m) => TermState m (CallCreated m pars name) where
  terminate (St callCreatedData) = clearCreatedCall callCreatedData
    
instance (CallProv m pars, MonadIO m) => Transition m (CallInitiated m pars name) where
  type NextStates (CallInitiated m pars name) = ListOfNamed St name (InitiatedTransitionNextStates m pars) 
  next st@(St callState) cb = initiatedTransition callState (\nextCallStates -> cb (toVarOfSt (nameFromSt st) nextCallStates))   

instance (CallProv m pars, MonadIO m) => Transition m (CallRingback m pars name) where
  type NextStates (CallRingback m pars name) = ListOfNamed St name (RingbackTransitionNextStates m pars) 
  next st@(St callState) cb = ringbackTransition callState (\nextCallStates -> cb (toVarOfSt (nameFromSt st) nextCallStates))   

instance (CallProv m pars, MonadIO m) => Transition m (CallConnected m pars bridgeState muteState mediaState name) where
  type NextStates (CallConnected m pars bridgeState muteState mediaState name) = ListOfNamed St name (ConnectedTransitionNextStates m pars) 
  next st@(St callState) cb = connectedTransition callState (\nextCallStates -> cb (toVarOfSt (nameFromSt st) nextCallStates))   

instance (CallProv m pars, MonadIO m) => Transition m (CallOffered m pars name) where
  type NextStates (CallOffered m pars name) = ListOfNamed St name (OfferedTransitionNextStates m pars) 
  next st@(St callState) cb = offeredTransition callState (\nextCallStates -> cb (toVarOfSt (nameFromSt st) nextCallStates))   

instance (CallProv m pars, MonadIO m) => Transition m (CallAnswering m pars name) where
  type NextStates (CallAnswering m pars name) = ListOfNamed St name (AnsweringTransitionNextStates m pars) 
  next st@(St callState) cb = answeringTransition callState (\nextCallStates -> cb (toVarOfSt (nameFromSt st) nextCallStates))   

instance (CallProv m pars, MonadIO m) => Transition m (CallAccepted m pars name) where
  type NextStates (CallAccepted m pars name) = ListOfNamed St name (AcceptedTransitionNextStates m pars) 
  next st@(St callState) cb = acceptedTransition callState (\nextCallStates -> cb (toVarOfSt (nameFromSt st) nextCallStates))   
    
instance (CallProv m pars, MonadIO m) => CreateRes m name (CallRes pars) (V '[CallCreated m pars name]) where
  createRes _named (CallRes callResPars) = fmap (variantFromValue . St) (createCall callResPars)  

instance (CallProv m pars, MonadIO m) => CreateRes m name (IncomingCallRes pars) (V '[CallOffered m pars name]) where
  createRes _named (IncomingCallRes incomingCallResPars channelHandle) = fmap (variantFromValue . St) (createIncomingCall incomingCallResPars channelHandle)  
  
instance (CallProv m pars, MonadIO m) => MkRes m (IncomingCallRes pars) where
  type ResSt m (IncomingCallRes pars)  = CallOfferedData m pars
  mkRes (IncomingCallRes incomingCallResPars channelHandle) = createIncomingCall incomingCallResPars channelHandle
  
instance (CallProv m pars, MonadIO m) => MkRes m (CallRes pars) where
  type ResSt m (CallRes pars)  = CallCreatedData m pars
  mkRes (CallRes callResPars) = createCall callResPars
  
instance (CallProv m pars, MonadIO m) => Request m MakeCall (CallCreated m pars name) where
  type ReqResult MakeCall (CallCreated m pars name) = '[CallInitiated m pars name,CallFailed m pars name] 
  request makeCallReq st@(St callData) = fmap (toVarOfSt (nameFromSt st)) (makeCall makeCallReq callData)

instance (CallProv m pars, MonadIO m) => Request m DropCall (CallInitiated m pars name) where
  type ReqResult DropCall (CallInitiated m pars name) = '[CallDisconnected m pars name] 
  request dropCallReq (St callData) =  fmap (variantFromValue . St) (dropInitiateCall dropCallReq callData)
  
instance (CallProv m pars, MonadIO m) => Request m DropCall (CallRingback m pars name) where
  type ReqResult DropCall (CallRingback m pars name) = '[CallDisconnected m pars name] 
  request dropCallReq (St callData) =  fmap (variantFromValue . St) (dropRingbackCall dropCallReq callData)
    
instance (CallProv m pars, MonadIO m) => Request m DropCall (CallConnected m pars bridgeState muteState mediaState name) where
  type ReqResult DropCall (CallConnected m pars bridgeState muteState mediaState name) = '[CallDisconnected m pars name] 
  request dropCallReq (St callData) =  fmap (variantFromValue . St) (dropConnectedCall dropCallReq callData)
      
instance (CallProv m pars, MonadIO m) => Request m DropCall (CallOffered m pars name) where
  type ReqResult DropCall (CallOffered m pars name) = '[CallDisconnected m pars name] 
  request dropCallReq (St callData) =  fmap (variantFromValue . St) (dropOfferedCall dropCallReq callData)

instance (CallProv m pars, MonadIO m) => Request m DropCall (CallAccepted m pars name) where
  type ReqResult DropCall (CallAccepted m pars name) = '[CallDisconnected m pars name] 
  request dropCallReq (St callData) =  fmap (variantFromValue . St) (dropAcceptedCall dropCallReq callData)

instance (CallProv m pars, MonadIO m) => Request m DropCall (CallAnswering m pars name) where
  type ReqResult DropCall (CallAnswering m pars name) = '[CallDisconnected m pars name] 
  request dropCallReq (St callData) =  fmap (variantFromValue . St) (dropAnsweringCall dropCallReq callData)

instance (CallProv m pars, MonadIO m) => Request m AnswerCall (CallOffered m pars name) where
  type ReqResult AnswerCall (CallOffered m pars name) = '[CallAnswering m pars name, CallDisconnected m pars name] 
  request answerCallReq st@(St callData) =  fmap (toVarOfSt (nameFromSt st)) (answerOfferedCall answerCallReq callData) 

instance (CallProv m pars, MonadIO m) => Request m AnswerCall (CallAccepted m pars name) where
  type ReqResult AnswerCall (CallAccepted m pars name) = '[CallAnswering m pars name, CallDisconnected m pars name] 
  request answerCallReq st@(St callData) =  fmap (toVarOfSt (nameFromSt st)) (answerAcceptedCall answerCallReq callData) 

instance (CallProv m pars, MonadIO m) => Request m AcceptCall (CallOffered m pars name) where
  type ReqResult AcceptCall (CallOffered m pars name) = '[CallAccepted m pars name, CallDisconnected m pars name] 
  request acceptCallReq st@(St callData) =  fmap (toVarOfSt (nameFromSt st)) (acceptCall acceptCallReq callData) 
    
instance (CallProv m pars, MonadIO m) => Request m MuteCall (CallConnected m pars bridgeState 'MuteOff mediaState name) where
  type ReqResult MuteCall (CallConnected m pars bridgeState 'MuteOff mediaState name) = '[CallConnected m pars bridgeState 'MuteOn mediaState name] 
  request muteCallReq (St callData) =  fmap (variantFromValue . St) (muteCall muteCallReq callData)
        
instance (CallProv m pars, MonadIO m) => Request m UnmuteCall (CallConnected m pars bridgeState 'MuteOn mediaState name) where
  type ReqResult UnmuteCall (CallConnected m pars bridgeState 'MuteOn mediaState name) = '[CallConnected m pars bridgeState 'MuteOff mediaState name] 
  request unmuteCallReq (St callData) =  fmap (variantFromValue . St) (unmuteCall unmuteCallReq callData)
        
instance (CallProv m pars, MonadIO m) => Request m AddToBridge (CallConnected m pars 'BridgeOff muteState 'NoMedia name) where
  type ReqResult AddToBridge (CallConnected m pars 'BridgeOff muteState 'NoMedia name) = '[CallConnected m pars 'BridgeOn muteState 'NoMedia name] 
  request addToBridgeReq (St callData) =  fmap (variantFromValue . St) (addToBridge addToBridgeReq callData)
        
instance (CallProv m pars, MonadIO m) => Request m RemoveFromBridge (CallConnected m pars 'BridgeOn muteState 'NoMedia name) where
  type ReqResult RemoveFromBridge (CallConnected m pars 'BridgeOn muteState 'NoMedia name) = '[CallConnected m pars 'BridgeOff muteState 'NoMedia name] 
  request removeFromBridgeReq (St callData) =  fmap (variantFromValue . St) (removeFromBridge removeFromBridgeReq callData)
        
instance (CallProv m pars, MonadIO m) => Request m PlayRingback (CallConnected m pars 'BridgeOff muteState 'NoMedia name) where
  type ReqResult PlayRingback (CallConnected m pars 'BridgeOff muteState 'NoMedia name) = '[CallConnected m pars 'BridgeOff muteState 'PlayingRingback name] 
  request playRingbackReq (St callData) =  fmap (variantFromValue . St) (playRingback playRingbackReq callData)
        
instance (CallProv m pars, MonadIO m) => Request m StopRingback (CallConnected m pars 'BridgeOff muteState 'PlayingRingback name) where
  type ReqResult StopRingback (CallConnected m pars 'BridgeOff muteState 'PlayingRingback name) = '[CallConnected m pars 'BridgeOff muteState 'NoMedia name] 
  request stopRingbackReq (St callData) =  fmap (variantFromValue . St) (stopRingback stopRingbackReq callData)
        
instance (CallProv m pars, MonadIO m) => Request m PlayMusic (CallConnected m pars 'BridgeOff muteState 'NoMedia name) where
  type ReqResult PlayMusic (CallConnected m pars 'BridgeOff muteState 'NoMedia name) = '[CallConnected m pars 'BridgeOff muteState 'PlayingMusic name] 
  request playMusicReq (St callData) =  fmap (variantFromValue . St) (playMusic playMusicReq callData)
        
instance (CallProv m pars, MonadIO m) => Request m StopMusic (CallConnected m pars 'BridgeOff muteState 'PlayingMusic name) where
  type ReqResult StopMusic (CallConnected m pars 'BridgeOff muteState 'PlayingMusic name) = '[CallConnected m pars 'BridgeOff muteState 'NoMedia name] 
  request stopMusicReq (St callData) =  fmap (variantFromValue . St) (stopMusic stopMusicReq callData)

instance (CallProv m pars, MonadIO m) => Request m StartMediaSession (CallConnected m pars 'BridgeOff muteState 'NoMedia name) where
  type ReqResult StartMediaSession (CallConnected m pars 'BridgeOff muteState 'NoMedia name) = '[CallConnected m pars 'BridgeOff muteState 'MediaSession name] 
  request startMediaReq (St callData) =  fmap (variantFromValue . St) (startMediaSession startMediaReq callData)
        
instance (CallProv m pars, MonadIO m) => Request m StopMediaSession (CallConnected m pars 'BridgeOff muteState 'MediaSession name) where
  type ReqResult StopMediaSession (CallConnected m pars 'BridgeOff muteState 'MediaSession name) = '[CallConnected m pars 'BridgeOff muteState 'NoMedia name] 
  request stopMediaReq (St callData) =  fmap (variantFromValue . St) (stopMediaSession stopMediaReq callData)
  
getCallMediaHandle :: CallProv m pars => CallConnected m pars 'BridgeOff muteState 'MediaSession name -> m MediaHandle 
getCallMediaHandle (St callData) = getChannelMediaHandle callData

--
type instance Eval (SupportedRequests (CallOffered m pars name)) =    '[AcceptCall, AnswerCall, DropCall]
type instance Eval (SupportedRequests (CallAccepted m pars name)) =   '[AnswerCall, DropCall]
type instance Eval (SupportedRequests (CallCreated m pars name)) =    '[MakeCall]
type instance Eval (SupportedRequests (CallInitiated m pars name)) =  '[DropCall]
type instance Eval (SupportedRequests (CallRingback m pars name)) =   '[DropCall]
type instance Eval (SupportedRequests (CallAnswering m pars name)) =  '[DropCall]
type instance Eval (SupportedRequests (CallFailed m pars name)) =     '[]
type instance Eval (SupportedRequests (CallDisconnected m pars name)) = '[]
type instance Eval (SupportedRequests (CallConnected m pars bridgeState muteState mediaState name)) = 
  Concat (BridgeRequests bridgeState muteState mediaState) 
    (Concat (MuteRequests bridgeState muteState mediaState)
      (Concat (MediaRequests bridgeState muteState mediaState) '[DropCall]))

type family MuteRequests (bridgeState :: BridgeState) (muteState :: MuteState) (mediaState :: MediaState) :: [*] where
   MuteRequests bridgeState 'MuteOff mediaState =   '[MuteCall]
   MuteRequests bridgeState 'MuteOn mediaState =    '[UnmuteCall]
   MuteRequests bridgeState muteState mediaState =  '[]

type family BridgeRequests (bridgeState :: BridgeState) (muteState :: MuteState) (mediaState :: MediaState) :: [*] where
   BridgeRequests 'BridgeOff muteState 'NoMedia =     '[AddToBridge]
   BridgeRequests 'BridgeOn muteState 'NoMedia =      '[RemoveFromBridge]
   BridgeRequests bridgeState muteState mediaState =  '[]

type family MediaRequests (bridgeState :: BridgeState) (muteState :: MuteState) (mediaState :: MediaState) :: [*] where
   MediaRequests 'BridgeOff muteState 'NoMedia =               '[PlayRingback, PlayMusic, StartMediaSession]
   MediaRequests 'BridgeOff muteState 'PlayingRingback =       '[StopRingback]
   MediaRequests 'BridgeOff muteState 'PlayingMusic =          '[StopMusic]
   MediaRequests 'BridgeOff muteState 'MediaSession =          '[StopMediaSession]
   MediaRequests bridgeState muteState mediaState =            '[]


type instance StateTitle (CallCreatedData m pars) = "CallCreated"
type instance StateTitle (CallOfferedData m pars) = "CallOffered"
type instance StateTitle (CallAcceptedData m pars) = "CallAccepted"
type instance StateTitle (CallAnsweringData m pars) = "CallAnswering"
type instance StateTitle (CallInitiatedData m pars) = "CallInitiated"
type instance StateTitle (CallFailedData m pars) = "CallFailed"
type instance StateTitle (CallRingbackData m pars) = "CallRingback"
type instance StateTitle (CallDisconnectedData m pars) = "CallDisconnected"
type instance StateTitle (CallConnectedData m pars bridgeState muteState mediaState) = 
  AppendSymbol "Connected" 
    (AppendSymbol (BridgeTitle bridgeState)
      (AppendSymbol (MuteTitle muteState) (MediaTitle mediaState)))


type family BridgeTitle (bridgeState :: BridgeState) where
  BridgeTitle 'BridgeOn = " Bridged"
  BridgeTitle _ = ""

type family MuteTitle (muteState :: MuteState) where
  MuteTitle 'MuteOn = " Muted"
  MuteTitle _ = ""
    
type family MediaTitle (mediaState :: MediaState) where
  MediaTitle 'PlayingRingback = " PlayingRingback"
  MediaTitle 'PlayingMusic = " PlayingMusic"
  MediaTitle 'MediaSession = " MediaSession"
  MediaTitle _ = ""
