module Main where

import ThisPrelude

import Effect.Console (error)
import Effect.Ref as Ref
import Nmailer (NMailer(..))
import Node.Process (exit)
import StateIO (loadState)
import KvmMail.Server.Express (runServer)
import KvmMail.Server.Services.RecentlyChanged (runRecentlyChangedService)
import KvmMail.Server.Services.SingletonRepo (SingFileRepo(..))
import KvmMail.Server.Subscription.Repo (defaultFileRepo)
import KvmMail.Server.Subscription.Usecases (runSubscriptionNotificationService)
import KvmMail.Server.Util (dotenvConfig, getApiBaseUrl, getHost, getNodeEnv, getPort, getUsers, isOther, unwrapOrExit, unwrapOrExitMb)
import KvmMail.Shared.JsonCodecs (settingsCdc)

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

  let
    env =
      { subscription: { repo: defaultFileRepo, mailer: NMailer unit, apiBaseUrl }
      , settingsRepo: SingFileRepo { filename: "settings.json", codec: settingsCdc }
      }

  runRecentlyChangedService
  runSubscriptionNotificationService # flip runReaderT env
  _ <- runServer host port stateRef env
  pure unit

