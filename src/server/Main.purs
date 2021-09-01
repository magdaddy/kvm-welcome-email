module Main where

import ThisPrelude

import Effect.Console (error)
import Effect.Ref as Ref
import Node.Process (exit)
import StateIO (loadState)
import WelcomeEmail.Server.Express (runServer)
import WelcomeEmail.Server.Services.RecentlyChanged (runRecentlyChangedService)
import WelcomeEmail.Server.Subscription.Usecases (runSubscriptionNotificationService)
import WelcomeEmail.Server.Util (dotenvConfig, getApiBaseUrl, getHost, getNodeEnv, getPort, getUsers, isOther, unwrapOrExit, unwrapOrExitMb)

main :: Effect Unit
main = do
  dotenvConfig
  nodeEnv <- getNodeEnv
  when (isOther nodeEnv) do
    error "NODE_ENV is not set to 'production' or 'development'."
    error "Please set it in .env or on the command line."
    exit 1
  host <- getHost >>= unwrapOrExitMb
    "HOST is not set. Please set it in .env or on the command line."
  port <- getPort >>= unwrapOrExitMb
    "PORT is not set. Please set it in .env or on the command line."
  _users <- getUsers >>= unwrapOrExit "Users could not be loaded"
  savedState <- loadState >>= unwrapOrExit "Saved state could not be loaded"
  stateRef <- Ref.new { running: Nothing, saved: savedState }
  apiBaseUrl <- getApiBaseUrl >>= unwrapOrExitMb
    "API_BASE_URL is not set. Please set it in .env or on the command line."

  runRecentlyChangedService
  runSubscriptionNotificationService apiBaseUrl
  _ <- runServer host port stateRef { apiBaseUrl }
  -- runSocketIo httpServer stateRef
  pure unit

