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

{-# OPTIONS_GHC -freduction-depth=0 #-}

module Main where

import           Beseder.ARI.ARICallProv 
import           ARICore    
import           Beseder.ARI.ARIRunner
import           Data.Text
import           Protolude                    hiding (Product, handle, return, gets, lift, liftIO,
                                               (>>), (>>=), forever, until, try, on)
import DialTimerApp
import SimRingCallApp2
import TwoWayCallApp
import Utils

ariSrvAddress :: Text
ariSrvAddress = "192.168.2.200"

applName::Text
applName="hello-world"

config :: Text -> ARIConfig
config srvAddress = ARIConfig (unpack applName) (unpack srvAddress) "user" "pwd" 8088 

cfg :: ARIConfig
cfg = config ariSrvAddress

main :: IO ()
main = do 
  setupLog2
  let  makeCallReq1 = MakeCall 
        { tech = SIP
        , prov = Provider "192.168.2.26"
        , callerID = Party ("100"::Text)
        , dest = Party ("400"::Text)
        }

  res <- runWithServerDataOnce cfg (timerAppArr1 makeCallReq1 120)
  print res 


main2 :: IO ()
main2 = do 
  let makeCallReq1 = MakeCall 
          { tech = SIP
          , prov = Provider "192.168.2.26"
          , callerID = Party ("100"::Text)
          , dest = Party ("300"::Text) -- "phn_201"::Text)
          }
      makeCallReq2 = MakeCall 
          { tech = SIP
          , prov = Provider "192.168.2.26"
          , callerID = Party ("100"::Text)
          , dest = Party ("phn_201"::Text)
          }
   
  res <- runWithServerDataOnce cfg (twoWayCallApp makeCallReq1 makeCallReq2 35)
  print res 

main3 :: IO ()
main3 = do 
  let makeCallReq1 = MakeCall 
          { tech = SIP
          , prov = Provider "192.168.2.26"
          , callerID = Party ("100"::Text)
          , dest = Party ("300"::Text) -- "phn_201"::Text)
          }
      makeCallReq2 = MakeCall 
          { tech = SIP
          , prov = Provider "192.168.2.26"
          , callerID = Party ("100"::Text)
          , dest = Party ("phn_201"::Text)
          }
      makeCallReq3 = MakeCall 
          { tech = SIP
          , prov = Provider "192.168.2.26"
          , callerID = Party ("100"::Text)
          , dest = Party ("400"::Text)
          }
  res <- runWithServerDataOnce cfg (simRingCallApp2 makeCallReq1 makeCallReq2 makeCallReq3 10 40)
  print res 
