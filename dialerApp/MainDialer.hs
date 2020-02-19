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
import SimRingCallApp
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
          , dest = Party ("200"::Text)
          }
      makeCallReq3 = MakeCall 
          { tech = SIP
          , prov = Provider "192.168.2.26"
          , callerID = Party ("100"::Text)
          , dest = Party ("400"::Text)
          }
  res <- runWithServerDataOnce cfg (simRingCallApp makeCallReq1 makeCallReq2 makeCallReq3 10 40)
  print res 

{-
[12 of 12] Compiling Main             ( dialerApp\MainDialer.hs, interpreted )
*** Parser [Main]:
!!! Parser [Main]: finished in 15.63 milliseconds, allocated 4.305 megabytes
*** Renamer/typechecker [Main]:
  = {terms: 17,854,
     types: 9,647,856,
     coercions: 410,993,623,
     joins: 0/5}
!!! CorePrep [SimRingCallApp2]: finished in 363656.25 milliseconds, allocated 24.506 megabytes
*** ByteCodeGen [SimRingCallApp2]:
!!! ByteCodeGen [SimRingCallApp2]: finished in 328.12 milliseconds, allocated 61.356 megabytes
Upsweep completely successful.
*** Deleting temp files:
Warning: deleting non-existent C:\Users\boris\AppData\Local\Temp\ghc2560_0\ghc_53.c
Ok, 9 modules loaded.
*SimRingCallApp2> !!! Renamer/typechecker [Main]: finished in 457921.88 milliseconds, allocated 118289.815 megabytes
*** Desugar [Main]:
Result size of Desugar (before optimization)
  = {terms: 36,623,
     types: 14,869,853,
     coercions: 420,319,361,
     joins: 0/30}
Result size of Desugar (after optimization)
  = {terms: 23,423,
     types: 10,052,182,
     coercions: 415,451,318,
     joins: 0/6}
!!! Desugar [Main]: finished in 989531.25 milliseconds, allocated 183610.965 megabytes
*** Simplifier [Main]:
Result size of Simplifier iteration=1
  = {terms: 24,711,
     types: 10,269,995,
     coercions: 414,232,837,
     joins: 0/6}
Result size of Simplifier
  = {terms: 24,615,
     types: 10,269,899,
     coercions: 414,226,783,
     joins: 0/6}
!!! Simplifier [Main]: finished in 671812.50 milliseconds, allocated 86767.181 megabytes
*** CoreTidy [Main]:
Result size of Tidy Core
  = {terms: 24,615,
     types: 10,269,899,
     coercions: 414,226,783,
     joins: 0/6}
!!! CoreTidy [Main]: finished in 666171.88 milliseconds, allocated 51268.523 megabytes
*** CorePrep [Main]:
Result size of CorePrep
  = {terms: 24,768,
     types: 10,270,574,
     coercions: 414,226,783,
     joins: 0/64}
!!! CorePrep [Main]: finished in 232296.88 milliseconds, allocated 34.125 megabytes
*** ByteCodeGen [Main]:
!!! ByteCodeGen [Main]: finished in 515.62 milliseconds, allocated 86.592 megabytes
Upsweep completely successful.
*** Deleting temp files:
Warning: deleting non-existent C:\Users\boris\AppData\Local\Temp\ghc19476_0\ghc_24.c
Ok, 12 modules loaded.
-}