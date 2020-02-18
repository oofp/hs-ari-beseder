{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Beseder.ARI.DTMFMonitor 
  ( dtmfMonitor
  , anyDigitRes
  ) where

import           Protolude
import           Beseder.Resources.Task
import           Beseder.ARI.ARIData
import           Beseder.ARI.DTMFContainer
import           qualified Data.Sequence as Sq 

dtmfMonitor :: MediaHandle -> (Seq DTMFDigit -> Maybe (a, Seq DTMFDigit)) -> AsyncTaskPar m a  
dtmfMonitor (MkMediaHandle _ dtmfContainer) filterFunc = 
  asyncTaskResource  (atomically $ detectPattern filterFunc dtmfContainer)
  
anyDigitRes :: MediaHandle -> AsyncTaskPar m ()  
anyDigitRes mediaHandle = dtmfMonitor mediaHandle (\dtmfs -> if not (null dtmfs) then Just ((), Sq.empty) else Nothing)   

