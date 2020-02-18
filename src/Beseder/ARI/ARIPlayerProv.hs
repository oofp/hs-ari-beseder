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
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE TemplateHaskell       #-}

module Beseder.ARI.ARIPlayerProv where

import           Haskus.Utils.Variant
import           ARICore    
import           Protolude hiding (TypeError)
import           Beseder.Base.Base
import           Beseder.Base.Common
import           Beseder.ARI.ARIData
import           Beseder.Resources.ResourceDef

type PlayerTrans m fromState toStates = fromState -> (V toStates -> m Bool) -> m Bool   

data StopPlayer = StopPlayer deriving (Show, Eq)
data PlayMedia = PlayMedia PBMedia MediaHandle deriving Show
data PausePlayer = PausePlayer deriving (Show, Eq)
data ResumePlayer = ResumePlayer deriving (Show, Eq)

instance GetInstance StopPlayer where
  getInstance = StopPlayer

class Monad m => PlayerProv (m :: * -> *) pars where
  data PlayerCreated m pars
  data PlayerActive m pars
  data PlayerPaused m pars
  data PlayerStopped m pars
  data PlayerFailed m pars
  data PlayerCompleted m pars

  data ResPar m pars 

  createPlayer :: MkResDef m (ResPar m pars) (PlayerCreated m pars) 
  startPlayer :: RequestDef m PlayMedia (PlayerCreated m pars) '[PlayerActive m pars, PlayerFailed m pars]  
  stopPlayer :: RequestDef m StopPlayer (PlayerActive m pars)  '[PlayerStopped m pars]  
    
  clearFailedPlayer :: TermDef m (PlayerFailed m pars )
  clearStoppedPlayer ::  TermDef m (PlayerStopped m pars)
  clearCompletedPlayer :: TermDef m (PlayerCompleted m pars)
  clearPausedPlayer :: TermDef m (PlayerPaused m pars)
  clearCreatedPlayer :: TermDef m (PlayerCreated m pars)

  activeTransition ::  TransitionDef m  (PlayerActive m pars) '[PlayerCompleted m pars]

buildRes ''PlayerProv

type instance TermRequest (StPlayerActive m res name) = StopPlayer

