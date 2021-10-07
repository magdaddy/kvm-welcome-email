module KvmMail.App.RecentlyChangedPage where

import ThisPrelude

import Control.Monad.Except (runExceptT)
import Data.Tuple.Nested ((/\))
import Effect.Aff (launchAff_)
import MagLibs.DateFns (format)
import Network.RemoteData (RemoteData(..), fromEither)
import React.Basic.DOM (css)
import React.Basic.DOM as R
import React.Basic.Hooks (Component, JSX, component, useEffectOnce, useState')
import React.Basic.Hooks as React
import KvmMail.App.Api.Backend as Api
import KvmMail.App.Data (AppError(..), Page(..))
import KvmMail.Shared.Boundary (EntryChange)
import KvmMail.Shared.Template (entryLink)


mkRecentlyChangedPage :: Component { setPage :: Page -> Effect Unit }
mkRecentlyChangedPage = do
  component "RecentlyChangedPage" \props -> React.do
    (recentlyChanged :: RemoteData AppError (Array EntryChange)) /\ setRecentlyChanged <- useState' NotAsked

    useEffectOnce do
      launchAff_ do
        liftEffect $ setRecentlyChanged Loading
        resRecentlyChanged <- runExceptT Api.getRecentlyChanged
        case resRecentlyChanged of
          Left (Unauthorized err) -> do
            liftEffect $ log $ "Unauthorized: " <> err
            liftEffect $ props.setPage LoginPage
          _ -> liftEffect $ setRecentlyChanged $ fromEither resRecentlyChanged
      pure mempty

    pure $
      R.div
        { className: "page settings container is-max-desktop"
        , children:
          [ R.div
              { className: "box mt-5"
              , children:
                [ case recentlyChanged of
                    NotAsked -> R.text "Not asked"
                    Loading -> R.text "Loading..."
                    Failure e -> R.text $ show e
                    Success recentlyChanged' -> do
                      R.div
                        { className: ""
                        , style: css { height: "30rem", overflowY: "scroll" }
                        , children: map renderEntryChange recentlyChanged'
                        }
                ]
              }
          ]
        }


renderEntryChange :: EntryChange -> JSX
renderEntryChange ec = R.div
    { className: ""
    , children:
      [ R.span
          { className: "is-family-monospace mr-2"
          , children: [ R.text $ format "yy-MM-dd HH:mm:ss" ec.changed ]
          }
      , R.a
          { href: entryLink ec.entry
          , target: "_blank"
          , rel: "noopener noreferrer"
          , children: [ R.text ec.entry.title ]
          }
      , R.span { children: [ R.text $ " - v" <> show ec.entry.version ] }
      ]
    }
