module Beseder.ARI.DTMFContainer 
  ( newDTMFContainer
  , addDigit
  , lastDigit
  , clearDigits
  , detectPattern
  , DTMFContainer
  , DTMFDigit
  ) where

import           Control.Concurrent.STM.TVar    
import           Protolude
import           Data.Sequence 
import           qualified Data.Sequence as Sq 
import           Utils (DTMFDigit)

newtype DTMFData = DTMFData
  { collectedDigit :: Seq DTMFDigit
  } deriving (Eq, Show)

addDigit' :: DTMFDigit -> DTMFData -> DTMFData
addDigit' digit (DTMFData digits) = DTMFData (digits |> digit)

lastDigit' :: DTMFData -> Maybe DTMFDigit
lastDigit' (DTMFData Empty) = Nothing
lastDigit' (DTMFData (_hd :|> lastDig)) = Just lastDig

type DTMFContainer = TVar DTMFData

newDTMFContainer :: STM DTMFContainer
newDTMFContainer  = newTVar (DTMFData Sq.empty)

addDigit ::  DTMFDigit -> DTMFContainer -> STM ()
addDigit dtmfDigit dtmfCont = modifyTVar dtmfCont (addDigit' dtmfDigit) 

lastDigit :: DTMFContainer -> STM (Maybe DTMFDigit)
lastDigit dtmfCont = fmap lastDigit' (readTVar dtmfCont) 

detectPattern :: (Seq DTMFDigit -> Maybe (a, Seq DTMFDigit)) -> DTMFContainer -> STM a
detectPattern f dtmfCont = do
  (DTMFData digits) <- readTVar dtmfCont
  case f digits of
    Nothing -> retry
    Just (a, newDigits) -> do
      writeTVar dtmfCont (DTMFData newDigits) 
      return a

clearDigits :: DTMFContainer -> STM ()
clearDigits dtmfCont = writeTVar dtmfCont (DTMFData Sq.empty)

