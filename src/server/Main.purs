module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (error, log)
import Effect.Ref as Ref
import Node.Process (exit)
import StateIO (loadState)
import WelcomeEmail.Server.Express (runServer)
import WelcomeEmail.Server.Util (dotenvConfig, getNodeEnv, getUsers, isOther, tokenSecret, unwrapOrExit)


main :: Effect Unit
main = do
  dotenvConfig
  nodeEnv <- getNodeEnv
  when (isOther nodeEnv) do
    error "NODE_ENV is not set to 'production' or 'development'."
    error "Please set it in .env or on the command line."
    exit 1
  _users <- getUsers >>= unwrapOrExit "Users could not be loaded"
  -- _ <- getUsers'
  savedState <- loadState >>= unwrapOrExit "Saved state could not be loaded"
  stateRef <- Ref.new { running: Nothing, saved: savedState }
  log tokenSecret
  -- launchAff_ do
  --   theloop stateRef
  runServer stateRef
