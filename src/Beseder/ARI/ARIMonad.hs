{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}  
{-# LANGUAGE GeneralizedNewtypeDeriving    #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module  Beseder.ARI.ARIMonad 
    ( AriM 
    , initStateWithDistr
    , StateWithEvDistr
    , addARIEventSourceMonitor
    , runAriM
    , runAriM'
    ) 
    where

import           ARICore    
import           FlowTools
import           Utils (newEventDistributor, EventID, EventMonitor (..))
import           Protolude
import           Beseder.Base.Common
import           Control.Lens

data StateWithEvDistr s =  StateWithEvDistr {_ariEvDistr ::  ARIEventDistributor (StateWithEvDistr s), _extraData :: s}
makeLenses ''StateWithEvDistr

instance HasARIEventDistributor (StateWithEvDistr s)  where
  getEventDistributor = ariEvDistr

initStateWithDistr :: s -> StateWithEvDistr s 
initStateWithDistr s = StateWithEvDistr newEventDistributor s

type AriM s = FlowContextM (StateWithEvDistr s)  
          
instance TaskPoster (AriM s) where
    getTaskPoster = do
        fd <- ask
        let f hnd = submitTaskIO fd hnd 
        return f

addARIEventSourceMonitor :: (IsARIEventSource h) => h -> (ARISrcEvent -> AriM s Bool) -> AriM s EventID
addARIEventSourceMonitor h cbFunc = 
    let ariFilter (ARISrcEvent _ariEv src) = (src==createSource h)
    in addARIEventMonitor (EventMonitor ariFilter (\_ ev ->cbFunc ev >> return True))

runAriM :: ARIEnv ->  s -> AriM s Bool -> IO (Either FlowError (), StateWithEvDistr s)    
runAriM ariEnv s ariApp = do
    fd <- createFlowDataEnv ariEnv
    runFlowLoop ariApp fd (initStateWithDistr s)
    
runAriM' :: ARIEnv -> AriM () Bool -> IO (Either FlowError ())    
runAriM' ariEnv ariApp = fst <$> runAriM ariEnv () ariApp

