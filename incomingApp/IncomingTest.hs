{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Beseder.ARI.ARICallProv 
import           ARICore    
import           Beseder.ARI.ARIRunner
import           Data.Text
import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until, try, on)
import AnswerMicroIVRApp
-- import AnswerIncomingCallApp
import RouteIncomingCallApp
import Utils

ariSrvAddress :: Text
ariSrvAddress = "192.168.2.200"
applName::Text
applName="hello-world"

config :: Text -> ARIConfig
config srvAddress = ARIConfig (unpack applName) (unpack srvAddress) "boris" "boris" 8088 

cfg :: ARIConfig
cfg = config ariSrvAddress

main1 :: IO ()
main1 = do
  -- setupLog
  _ariEnv <- initCallServerData cfg (answerAndPlayApp  10)
  putStrLn ("Press <Enter> to exit"::Text)
  void getLine


main :: IO ()
main = do
  setupLog2
  let  makeCallReq = MakeCall 
        { tech = SIP
        , prov = Provider "192.168.2.26"
        , callerID = Party ("100"::Text)
        , dest = Party ("400"::Text)
        }
  _ariEnv <- initCallServerData cfg  (routeIncomingCallApp 30 makeCallReq)
  putStrLn ("Press <Enter> to exit"::Text)
  void getLine
  
  