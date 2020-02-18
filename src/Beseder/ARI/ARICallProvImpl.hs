{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE RecordWildCards       #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Beseder.ARI.ARICallProvImpl 
  ( ARICallRes
  , ariCallRes
  , AriCallAlive
  , AriCallCompleted
  , AriCallRingback
  , AriCallJustConnected
  , AriCallBridged
  , ARICall (..)
  ) where

import           Protolude
import           Haskus.Utils.Variant
import           ARICore    
import           FlowTools    
import           Beseder.ARI.ARIData
import           Control.Concurrent.STM.TVar
import           Beseder.ARI.DTMFContainer
import           Prelude (error)
import           Beseder.ARI.ARICallProv
import           Beseder.ARI.ARIMonad 

type ARICallRes  = CallRes ARICall
ariCallRes ::  ARICallRes
ariCallRes = CallRes ARICall

--Add Channel (with all the info to CallState, including CallerID)
data CallState s 
  = CallCreatedState
  | CallInitiatedState ChannelHandle (Maybe (V (InitiatedTransitionNextStates (AriM s) ARICall) -> AriM s Bool))
  | CallRingbackState ChannelHandle (Maybe (V (RingbackTransitionNextStates (AriM s) ARICall) -> AriM s Bool))
  | CallConnectedState ChannelHandle (Maybe BridgeHandle) DTMFContainer (Maybe (V (ConnectedTransitionNextStates (AriM s) ARICall) -> AriM s Bool))
  | CallOfferedState ChannelHandle (Maybe (V (OfferedTransitionNextStates (AriM s) ARICall) -> AriM s Bool))
  | CallAcceptedState ChannelHandle (Maybe (V (AcceptedTransitionNextStates (AriM s) ARICall) -> AriM s Bool))
  | CallAnsweringState ChannelHandle (Maybe (V (AnsweringTransitionNextStates (AriM s) ARICall) -> AriM s Bool))
  | CallDisconnectedState
  | CallFailedState

instance CallProv (AriM s) ARICall where
  data CallCreatedData (AriM s) ARICall = MkCallCreated (TVar (CallState s))
  data CallFailedData (AriM s) ARICall = MkCallFailed 
  data CallInitiatedData (AriM s) ARICall = MkCallInitiated (TVar (CallState s))
  data CallRingbackData (AriM s) ARICall =  MkCallRingback (TVar (CallState s))
  data CallConnectedData (AriM s) ARICall bridgeState muteState mediaState =  MkCallConnected (TVar (CallState s)) 
  data CallOfferedData (AriM s) ARICall =  MkCallOffered (TVar (CallState s))
  data CallAcceptedData (AriM s) ARICall =  MkCallAccepted (TVar (CallState s))
  data CallAnsweringData (AriM s) ARICall =  MkCallAnswering (TVar (CallState s))
  data CallDisconnectedData (AriM s) ARICall = MkCallDisconnected

  createCall _par = do
    callStateVar <- liftIO $ newTVarIO CallCreatedState
    return $ MkCallCreated callStateVar 

  createIncomingCall _par channelHandle = do
    void $ enableHanldeMonitoring channelHandle
    callStateVar <- liftIO $ newTVarIO (CallOfferedState channelHandle Nothing)
    void $ addARIEventSourceMonitor channelHandle (callEventHandler callStateVar)
    return $ MkCallOffered callStateVar 
    
  makeCall MakeCall {..} (MkCallCreated callStateVar) = do 
    chMaybe <- createHandleWithEvents (dialOutCmd tech prov callerID dest)
    case chMaybe of 
      Just ch -> do
        liftIO $ atomically $ writeTVar callStateVar (CallInitiatedState ch Nothing)
        void $ addARIEventSourceMonitor ch (callEventHandler callStateVar)
        return $ toVariant (MkCallInitiated callStateVar)
      Nothing -> do
        liftIO $ atomically $ writeTVar callStateVar CallFailedState
        return $ toVariantAt @1 MkCallFailed

  dropInitiateCall DropCall (MkCallInitiated callStateVar) = dropCallInternal callStateVar
  dropRingbackCall DropCall (MkCallRingback callStateVar) = dropCallInternal callStateVar
  dropConnectedCall DropCall (MkCallConnected callStateVar) = dropCallInternal callStateVar
  dropOfferedCall DropCall (MkCallOffered callStateVar) = dropCallInternal callStateVar
  dropAnsweringCall DropCall (MkCallAnswering callStateVar) = dropCallInternal callStateVar
  dropAcceptedCall DropCall (MkCallAccepted callStateVar) = dropCallInternal callStateVar

  answerOfferedCall AnswerCall (MkCallOffered callStateVar) = 
    answerCallInternal callStateVar

  answerAcceptedCall AnswerCall (MkCallAccepted callStateVar) = 
    answerCallInternal callStateVar
    
  acceptCall AcceptCall (MkCallOffered callStateVar) = do
    chanHandleMaybe <- getChannelHandleVar callStateVar
    case chanHandleMaybe of
      Nothing -> do 
        liftIO $ atomically $ writeTVar callStateVar CallDisconnectedState
        return $ toVariantAt @1 MkCallDisconnected -- unexpected
      Just channelHandle -> do
        void $ startRingingChannelCmd channelHandle
        liftIO $ atomically $ writeTVar callStateVar (CallAcceptedState channelHandle Nothing) 
        return $ toVariantAt @0 (MkCallAccepted callStateVar) 

  muteCall MuteCall (MkCallConnected callStateVar) = do
    chanHandleMaybe <- getChannelHandleVar callStateVar
    forM_ chanHandleMaybe (startMuteChannelCmd MuteDirOut)  
    return $ MkCallConnected callStateVar 
      
  unmuteCall UnmuteCall (MkCallConnected callStateVar) = do 
    chanHandleMaybe <- getChannelHandleVar callStateVar
    forM_ chanHandleMaybe (stopMuteChannelCmd MuteDirOut)   
    return $ MkCallConnected callStateVar

  addToBridge (AddToBridge bridgeHandle) (MkCallConnected callStateVar) = do
    chanHandleMaybe <- getChannelHandleVar callStateVar
    forM_ chanHandleMaybe (addChannelToBridgeCmd bridgeHandle)
    --TODO: consider clearing DTMFs here / liftIO $ atomically $ clearDigits dtmfCont
    setBridgeHandleVar bridgeHandle callStateVar 
    return $ MkCallConnected callStateVar

  removeFromBridge RemoveFromBridge (MkCallConnected callStateVar) = do
    chanAndBridgeHandlesMaybe <- getChannelAndBridgeHandlesVar callStateVar
    forM_ chanAndBridgeHandlesMaybe (\(channelHandle,bridgeHandle) -> removeChannelFromBridgeCmd bridgeHandle channelHandle)
    resetBridgeHandleVar callStateVar 
    return $ MkCallConnected callStateVar
    
  playRingback PlayRingback (MkCallConnected callStateVar) = do
    chanHandleMaybe <- getChannelHandleVar callStateVar
    forM_ chanHandleMaybe startRingingChannelCmd 
    return $ MkCallConnected callStateVar
  
  stopRingback StopRingback (MkCallConnected callStateVar) = do
    chanHandleMaybe <- getChannelHandleVar callStateVar
    forM_ chanHandleMaybe stopRingingChannelCmd 
    return $ MkCallConnected callStateVar

  playMusic PlayMusic (MkCallConnected callStateVar) = do
    chanHandleMaybe <- getChannelHandleVar callStateVar
    forM_ chanHandleMaybe startMOHChannelCmd 
    return $ MkCallConnected callStateVar
  
  stopMusic StopMusic (MkCallConnected callStateVar) = do
    chanHandleMaybe <- getChannelHandleVar callStateVar
    forM_ chanHandleMaybe stopMOHChannelCmd 
    return $ MkCallConnected callStateVar

  startMediaSession StartMediaSession (MkCallConnected callStateVar) = return (MkCallConnected callStateVar) 
  stopMediaSession StopMediaSession (MkCallConnected callStateVar) = return (MkCallConnected callStateVar) 

  getChannelMediaHandle (MkCallConnected callStateVar) = do
    chanHandleMaybe <- getChannelHandleVar callStateVar
    case chanHandleMaybe of 
      Just channelHandle -> do 
        callState <- liftIO $ readTVarIO callStateVar
        case callState of 
          CallConnectedState _ _ dtmfCont _ -> return $ MkMediaHandle channelHandle dtmfCont
          _ -> error "CallStateVar is not CallConnectedState"
      Nothing -> error "Channel Handle must be available at Connected state"


  clearDisconnectedCall _ = return ()
  clearCreatedCall _ = return ()
  clearFailedCall _ = return ()

  initiatedTransition (MkCallInitiated callStateVar) cb = do 
    liftIO $ atomically $ modifyTVar callStateVar 
      (\case 
        CallInitiatedState channelHandle _ ->  CallInitiatedState channelHandle (Just cb)
        unexpState -> unexpState
      ) 
    return True

  ringbackTransition (MkCallRingback callStateVar) cb = do 
    liftIO $ atomically $ modifyTVar callStateVar 
      (\case 
        CallRingbackState channelHandle _ ->  CallRingbackState channelHandle (Just cb)
        unexpState -> unexpState
      ) 
    return True

  answeringTransition (MkCallAnswering callStateVar) cb = do 
    liftIO $ atomically $ modifyTVar callStateVar 
      (\case 
        CallAnsweringState channelHandle _ ->  CallAnsweringState channelHandle (Just cb)
        unexpState -> unexpState
      ) 
    return True

  offeredTransition (MkCallOffered callStateVar) cb = do 
    liftIO $ atomically $ modifyTVar callStateVar 
      (\case 
        CallOfferedState channelHandle _ ->  CallOfferedState channelHandle (Just cb)
        unexpState -> unexpState
      ) 
    return True

  acceptedTransition (MkCallAccepted callStateVar) cb = do 
    liftIO $ atomically $ modifyTVar callStateVar 
      (\case 
        CallAcceptedState channelHandle _ ->  CallAcceptedState channelHandle (Just cb)
        unexpState -> unexpState
      ) 
    return True
  
  connectedTransition (MkCallConnected callStateVar) cb = do 
    liftIO $ atomically $ modifyTVar callStateVar 
      (\case 
        CallConnectedState channelHandle bhMaybe dtmfCont _ ->  CallConnectedState channelHandle bhMaybe dtmfCont (Just cb)
        unexpState -> unexpState
      ) 
    return True

------------  
type AriCallCompleted s name = CallCompleted (AriM s) ARICall name 
type AriCallAlive s name = CallAlive (AriM s) ARICall name
type AriCallRingback s name = CallRingback (AriM s) ARICall name
type AriCallJustConnected s name = CallConnected (AriM s) ARICall 'BridgeOff 'MuteOff 'NoMedia name
type AriCallBridged s name = CallConnected (AriM s) ARICall 'BridgeOn 'MuteOff 'NoMedia name

------------
getChannelHandleVar :: (MonadIO m) => TVar (CallState s) -> m (Maybe ChannelHandle)
getChannelHandleVar callStateVar = 
  fmap getChannelHandle (liftIO $ readTVarIO callStateVar)

getChannelAndBridgeHandlesVar :: (MonadIO m) => TVar (CallState s) -> m (Maybe (ChannelHandle, BridgeHandle))
getChannelAndBridgeHandlesVar callStateVar = 
  fmap getChannelAndBridgeHandle (liftIO $ readTVarIO callStateVar)

setBridgeHandleVar :: (MonadIO m) => BridgeHandle -> TVar (CallState s) -> m ()
setBridgeHandleVar bridgeHandle callStateVar =
  liftIO $ atomically $ modifyTVar callStateVar (setBridgeHandle bridgeHandle) 
  
resetBridgeHandleVar :: (MonadIO m) => TVar (CallState s) -> m ()
resetBridgeHandleVar callStateVar =
  liftIO $ atomically $ modifyTVar callStateVar resetBridgeHandle  
  
setBridgeHandle :: BridgeHandle -> CallState s -> CallState s 
setBridgeHandle bridgeHandle (CallConnectedState channelHandle _  dtmfContainer cb) = (CallConnectedState channelHandle (Just bridgeHandle) dtmfContainer cb) 
setBridgeHandle _ callState = callState

resetBridgeHandle :: CallState s -> CallState s 
resetBridgeHandle (CallConnectedState channelHandle _ dtmfContainer cb) = (CallConnectedState channelHandle Nothing dtmfContainer cb) 
resetBridgeHandle callState = callState 

getChannelHandle :: CallState s -> Maybe ChannelHandle
getChannelHandle (CallInitiatedState channelHandle _) = Just channelHandle
getChannelHandle (CallRingbackState channelHandle _) = Just channelHandle
getChannelHandle (CallConnectedState channelHandle _ _ _) = Just channelHandle
getChannelHandle (CallOfferedState channelHandle _) = Just channelHandle
getChannelHandle (CallAnsweringState channelHandle _) = Just channelHandle
getChannelHandle (CallAcceptedState channelHandle _) = Just channelHandle
getChannelHandle CallDisconnectedState = Nothing
getChannelHandle CallFailedState = Nothing
getChannelHandle CallCreatedState = Nothing

getChannelAndBridgeHandle :: CallState s -> Maybe (ChannelHandle, BridgeHandle)
getChannelAndBridgeHandle (CallConnectedState channelHandle (Just bridgeHandle) _ _) = Just (channelHandle, bridgeHandle)
getChannelAndBridgeHandle _ = Nothing

dropCallInternal :: TVar (CallState s) -> AriM s (CallDisconnectedData (AriM s) ARICall)
dropCallInternal callStateVar = do
  chanHandleMaybe <- getChannelHandleVar callStateVar
  forM_ chanHandleMaybe dropChannelCmd  
  liftIO $ atomically $ writeTVar callStateVar CallDisconnectedState
  return $ MkCallDisconnected 

answerCallInternal :: TVar (CallState s) -> AriM s (V [CallAnsweringData (AriM s) ARICall, CallDisconnectedData (AriM s) ARICall])   
answerCallInternal callStateVar = do
  chanHandleMaybe <- getChannelHandleVar callStateVar
  case chanHandleMaybe of
    Nothing -> do 
      liftIO $ atomically $ writeTVar callStateVar CallDisconnectedState
      return $ toVariantAt @1 MkCallDisconnected -- unexpected
    Just channelHandle -> do
      void $ answerChannelCmd channelHandle
      liftIO $ atomically $ writeTVar callStateVar (CallAnsweringState channelHandle Nothing) 
      return $ toVariantAt @0 (MkCallAnswering callStateVar) 

--------------------------      
data ChannelSt 
  = ChannelRingback
  | ChannelConnected
  | ChannelDisconnected
  deriving (Eq,Show)

channelStateFromEvent :: ChannelEventData -> Maybe ChannelSt
channelStateFromEvent (ChannelEventData chanEvType chan) = 
  case chanEvType of
    ChannelHangupRequest -> Just ChannelDisconnected
    ChannelDestroyed -> Just ChannelDisconnected
    ChannelStateChange -> case ARICore.state chan of
      Ringing -> Just ChannelRingback
      Up -> Just ChannelConnected
      _ -> Nothing
    _ -> Nothing  

callEventHandler :: TVar (CallState s) -> ARISrcEvent -> AriM s Bool
callEventHandler callStateVar (ARISrcEvent (ChannelEvent channelEvData) _src) = do
  liftIO $ putStrLn (("callEventHandler: "::Text) <> show channelEvData)
  case channelStateFromEvent channelEvData of
    Nothing -> return True
    (Just channelSt) -> callStateHandler callStateVar channelSt
callEventHandler callStateVar (ARISrcEvent (DTMFEvent (DTMFEventData _ digit _)) _src) = do
  callState <- liftIO $ readTVarIO callStateVar
  case callState of
    CallConnectedState _ _ dtmfCont _ -> liftIO $ atomically $ addDigit digit dtmfCont
    _ -> return () 
  return True
callEventHandler _ _ = return True

callStateHandler :: forall s. TVar (CallState s) -> ChannelSt -> AriM s Bool
callStateHandler callStateVar channelSt  = 
  do 
    liftIO $ putStrLn (("callStateHandler: "::Text) <> show channelSt)
    callState <- liftIO $ readTVarIO callStateVar 
    case (callState, channelSt) of 
      (CallInitiatedState hnd cbMaybe, ChannelRingback) -> do
        liftIO $ atomically $ writeTVar callStateVar (CallRingbackState hnd Nothing)  
        let newCallData = MkCallRingback callStateVar 
        forM_ cbMaybe (\cb -> cb (toVariant newCallData)) 
        return True
      (CallInitiatedState hnd cbMaybe, ChannelConnected) -> do
        dtmfCont <- liftIO $ atomically $ newDTMFContainer  
        liftIO $ atomically $ writeTVar callStateVar (CallConnectedState hnd Nothing dtmfCont Nothing)
        let newCallData :: CallConnectedData (AriM s) ARICall 'BridgeOff 'MuteOff 'NoMedia 
            newCallData = MkCallConnected callStateVar
        forM_ cbMaybe (\cb -> cb (toVariant newCallData)) 
        return True
      (CallInitiatedState _hnd cbMaybe, ChannelDisconnected) -> do
        liftIO $ atomically $ writeTVar callStateVar CallDisconnectedState 
        let newCallData :: CallDisconnectedData (AriM s) ARICall 
            newCallData = MkCallDisconnected 
        forM_ cbMaybe (\cb -> cb (toVariant newCallData)) 
        return True
      (CallRingbackState hnd cbMaybe, ChannelConnected) -> do
        dtmfCont <- liftIO $ atomically $ newDTMFContainer  
        liftIO $ atomically $ writeTVar callStateVar (CallConnectedState hnd Nothing dtmfCont Nothing)  
        let newCallData :: CallConnectedData (AriM s) ARICall 'BridgeOff 'MuteOff 'NoMedia 
            newCallData = MkCallConnected callStateVar 
        forM_ cbMaybe (\cb -> cb (toVariant newCallData)) 
        return True
      (CallRingbackState _hnd cbMaybe, ChannelDisconnected) -> do
        liftIO $ atomically $ writeTVar callStateVar CallDisconnectedState 
        let newCallData :: CallDisconnectedData (AriM s) ARICall 
            newCallData = MkCallDisconnected 
        forM_ cbMaybe (\cb -> cb (toVariant newCallData)) 
        return True
      (CallConnectedState _hnd _bridgeHnd _dtmfCont cbMaybe, ChannelDisconnected) -> do
        liftIO $ putStrLn (("callStateHandler: "::Text) <> show channelSt <> " at Connnected state; cb:" <> show (isJust cbMaybe))
        liftIO $ atomically $ writeTVar callStateVar CallDisconnectedState 
        let newCallData :: CallDisconnectedData (AriM s) ARICall 
            newCallData = MkCallDisconnected 
        forM_ cbMaybe (\cb -> cb (toVariant newCallData)) 
        return True
      (CallAnsweringState hnd cbMaybe, ChannelConnected) -> do
        liftIO $ putStrLn (("callStateHandler: "::Text) <> show channelSt <> " at Answering state; cb:" <> show (isJust cbMaybe))
        dtmfCont <- liftIO $ atomically $ newDTMFContainer
        liftIO $ atomically $ writeTVar callStateVar (CallConnectedState hnd Nothing dtmfCont Nothing)  
        let newCallData :: CallConnectedData (AriM s) ARICall 'BridgeOff 'MuteOff 'NoMedia 
            newCallData = MkCallConnected callStateVar 
        forM_ cbMaybe (\cb -> cb (toVariant newCallData)) 
        return True
      (CallAnsweringState _hnd cbMaybe, ChannelDisconnected) -> do
        liftIO $ atomically $ writeTVar callStateVar CallDisconnectedState 
        let newCallData :: CallDisconnectedData (AriM s) ARICall 
            newCallData = MkCallDisconnected 
        forM_ cbMaybe (\cb -> cb (toVariant newCallData)) 
        return True
      (CallOfferedState _hnd cbMaybe, ChannelDisconnected) -> do
        liftIO $ atomically $ writeTVar callStateVar CallDisconnectedState 
        let newCallData :: CallDisconnectedData (AriM s) ARICall 
            newCallData = MkCallDisconnected 
        forM_ cbMaybe (\cb -> cb (toVariant newCallData)) 
        return True
      (CallAcceptedState _hnd cbMaybe, ChannelDisconnected) -> do
        liftIO $ putStrLn (("callStateHandler: "::Text) <> show channelSt <> " at Accepted state; cb:" <> show (isJust cbMaybe))
        liftIO $ atomically $ writeTVar callStateVar CallDisconnectedState 
        let newCallData :: CallDisconnectedData (AriM s) ARICall 
            newCallData = MkCallDisconnected 
        forM_ cbMaybe (\cb -> cb (toVariant newCallData)) 
        return True
      _ -> return True


