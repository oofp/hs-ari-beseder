{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RebindableSyntax       #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels       #-}

module Beseder.ARI.DTMFMonitorHelper 
  ( clearDTMFs
  , firstDigitRes
  ) where

import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                              (>>), (>>=), forever, until,try,on)
import           Beseder.Base.Common
import           Beseder.Base.ControlData 
import           Beseder.Resources.Task
import           Beseder.ARI.ARIData
import           Beseder.ARI.DTMFContainer
import           Beseder.ARI.DTMFMonitor

clearDTMFs :: MediaHandle -> STransData m sp LiftIOFunc ()
clearDTMFs (MkMediaHandle _ dtmfContainer) = liftIO $ atomically $ clearDigits dtmfContainer

firstDigitRes :: Named name -> MediaHandle -> STransData m sp (ComposeFunc LiftIOFunc (NewResFunc (AsyncTaskPar m ()) name m)) ()
firstDigitRes named mediaHandle = do 
  clearDTMFs mediaHandle
  let digitMonRes :: AsyncTaskPar m ()
      digitMonRes = anyDigitRes mediaHandle
  newRes named digitMonRes

