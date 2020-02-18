{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}  
{-# LANGUAGE GeneralizedNewtypeDeriving    #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Beseder.ARI.ARIData 
  ( MediaHandle (..)
  , ARICall (..)
  ) 
  where

import           Protolude
import           ARICore
import           Beseder.ARI.DTMFContainer
import qualified Prelude  (Show (..))

data MediaHandle where
  MkMediaHandle :: (PBCapabale hnd, RecCapabale hnd) => hnd -> DTMFContainer -> MediaHandle     

instance Prelude.Show MediaHandle where
  show (MkMediaHandle _ _) = "MediaHandle"

data ARICall = ARICall deriving Show
