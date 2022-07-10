{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE LambdaCase      #-}

module Run
    ( runMain
    ) where

import           Control.Lens
import           System.Environment
import           System.Exit

import qualified Sctp

runMain :: IO ()
runMain = do
  args <- getArgs
  case args of
    ["recv"] -> Sctp.recvTest
    ["send"] -> Sctp.sendTest
    _ -> do
      putStrLn "Usage: $0 {recv|send}"
      exitFailure
