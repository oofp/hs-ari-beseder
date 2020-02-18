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
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE LambdaCase   #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Beseder.ARI.ARIPlayerProvImpl 
  ( ariPlayerRes 
  , ariPlayerRes' 
  ) where

import           Control.Concurrent.STM.TVar
import           Control.Monad.Cont
import           Haskus.Utils.Variant
import           FlowTools    
import           ARICore    
import           Protolude hiding (TypeError)
import           Beseder.ARI.ARIData
import           Beseder.ARI.ARIMonad
import           Beseder.ARI.ARIPlayerProv           

data PlayerState s 
  = PlayerCreatedState
  | PlayerActiveState PlaybackHandle (Maybe (V '[PlayerCompleted (AriM s) ARICall] -> AriM s ()))
  | PlayerPausedState PlaybackHandle 
  | PlayerStoppedState 
  | PlayerFailedState
  | PlayerCompletedState

ariPlayerRes :: ResPar (AriM ()) ARICall
ariPlayerRes = ariPlayerRes' @()

ariPlayerRes' :: forall s. ResPar (AriM s) ARICall
ariPlayerRes' = ARIPlayer

instance PlayerProv (AriM s) ARICall where
  data PlayerCreated (AriM s) ARICall = MkPlayerCreated (TVar (PlayerState s))
  data PlayerActive (AriM s) ARICall = MkPlayerActive (TVar (PlayerState s))
  data PlayerPaused (AriM s) ARICall = MkPlayerPaused (TVar (PlayerState s))
  data PlayerStopped (AriM s) ARICall = MkPlayerStopped  
  data PlayerFailed (AriM s) ARICall = MkPlayerFailed 
  data PlayerCompleted (AriM s) ARICall = MkPlayerCompleted 
  data ResPar (AriM s) ARICall = ARIPlayer 

  createPlayer ARIPlayer = do 
    playerStateVar <- liftIO $ newTVarIO PlayerCreatedState
    return $ MkPlayerCreated playerStateVar 

  startPlayer (PlayMedia pbMedia (MkMediaHandle hnd _dtmfCont)) (MkPlayerCreated playerVar) = do
    pbHndMaybe <- createHandleWithEvents (\evHandler -> startPlaybackCmd pbMedia evHandler hnd)
    case pbHndMaybe of 
      Just pbHnd -> do
        liftIO $ atomically $ writeTVar playerVar (PlayerActiveState pbHnd Nothing)
        void $ addARIEventSourceMonitor pbHnd (playerEventHandler playerVar)
        return $ toVariant (MkPlayerActive playerVar)
      Nothing -> do
        liftIO $ atomically $ writeTVar playerVar PlayerFailedState
        return $ toVariantAt @1 MkPlayerFailed

  stopPlayer StopPlayer (MkPlayerActive playerVar) = do
    playerState <- liftIO $ readTVarIO playerVar
    let pbHandleMaybe = getPlayerHandle playerState
    forM_ pbHandleMaybe stopPlaybackCmd
    liftIO $ atomically $ writeTVar playerVar PlayerStoppedState
    return $ variantFromValue MkPlayerStopped 


  clearFailedPlayer  MkPlayerFailed = return ()
  clearStoppedPlayer MkPlayerStopped = return ()
  clearCompletedPlayer MkPlayerCompleted = return ()
  clearPausedPlayer (MkPlayerPaused playerVar) = do
    playerState <- liftIO $ readTVarIO playerVar
    let pbHandleMaybe = getPlayerHandle playerState
    forM_ pbHandleMaybe stopPlaybackCmd
    liftIO $ atomically $ writeTVar playerVar PlayerStoppedState
    return () 
  clearCreatedPlayer (MkPlayerCreated _playerVar) = return ()

  activeTransition (MkPlayerActive playerVar) cb = do
    liftIO $ atomically $ modifyTVar playerVar 
      (\case 
        PlayerActiveState playbackHandle _ ->  PlayerActiveState playbackHandle (Just cb)
        unexpState -> unexpState
      ) 
    
playerEventHandler :: TVar (PlayerState s) -> ARISrcEvent -> AriM s Bool
playerEventHandler playerVar ariSrcEvent = do
  liftIO $ putStrLn (("playerEventHandler: "::Text) <> show ariSrcEvent)
  playerState <- liftIO $ readTVarIO playerVar 
  case (playerState, ariSrcEvent) of 
    (PlayerActiveState _pbHandle cbMaybe, ARISrcEvent (PlaybackEvent (PlaybackEventData PlaybackFinished _ )) _src) -> do
      liftIO $ atomically $ writeTVar playerVar PlayerCompletedState   
      let newPlayerData = MkPlayerCompleted 
      forM_ cbMaybe (\cb -> cb (variantFromValue newPlayerData)) 
      return True
    _ -> return True  


getPlayerHandle :: PlayerState s -> Maybe PlaybackHandle
getPlayerHandle (PlayerActiveState playbackHandle _) = Just playbackHandle
getPlayerHandle (PlayerPausedState playbackHandle) = Just playbackHandle
getPlayerHandle _ = Nothing    