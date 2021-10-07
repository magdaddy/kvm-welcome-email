module KvmMail.App.App where

import ThisPrelude

import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Foreign.Object (insert, singleton)
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Hooks (Component, component, useEffectOnce, useState')
import React.Basic.Hooks as React
import KvmMail.App.Api.Web (removeTokenFromLocalStorage)
import KvmMail.App.Data (Page(..))
import KvmMail.App.LoginPage (mkLoginPage)
import KvmMail.App.RecentlyChangedPage (mkRecentlyChangedPage)
import KvmMail.App.SettingsPage (mkSettingsPage)
import KvmMail.App.StatusPage (mkStatusPage)
import KvmMail.App.TemplatePage (mkTemplatePage)
import KvmMail.Shared.Boundary (defaultSettings)


gPage = StatusPage :: Page

mkApp :: Component Unit
mkApp = do
  bulmaNavbar <- mkBulmaNavbar
  statusPage <- mkStatusPage
  templatePage <- mkTemplatePage
  settingsPage <- mkSettingsPage
  loginPage <- mkLoginPage
  recentlyChangedPage <- mkRecentlyChangedPage
  component "App" \_ -> React.do
    (page :: Page) /\ setPage <- useState' gPage
    useEffectOnce do
      pure mempty
    pure $
      R.div
        { id: "app"
        , children:
          [ bulmaNavbar { setPage }
          , case page of
              StatusPage -> statusPage { setPage }
              TemplatePage -> templatePage { setPage, defaultEntry: defaultSettings.defaultEntry }
              SettingsPage -> settingsPage { setPage }
              LoginPage -> loginPage { setPage }
              RecentlyChangedPage -> recentlyChangedPage { setPage }
          ]
        }


mkBulmaNavbar :: Component { setPage :: Page -> Effect Unit }
mkBulmaNavbar = do
  component "BulmaNavbar" \props -> React.do
    let
      aria = insert "expanded" "false" $ singleton "label" "menu"
      navbarBurger =
        R.a
          { className: "navbar-burger"
          , role: "button"
          , _aria: aria
          , children:
            [ R.span { _aria: singleton "hidden" "true" }
            , R.span { _aria: singleton "hidden" "true" }
            , R.span { _aria: singleton "hidden" "true" }
            ]
          }
    pure $
      R.nav
        { className: "navbar"
        , role: "navigation"
        , _aria: singleton "label" "main navigation"
        , children:
          [ R.div
              { className: "navbar-brand"
              , children:
                [ R.a
                    { className: "navbar-item"
                    , children: [ R.text "KVM Mail" ]
                    , onClick: handler_ $ props.setPage StatusPage
                    }
                , navbarBurger
                ]
              }
          , R.div
              { className: "navbar-menu"
              , children:
                [ R.div
                    { className: "navbar-start"
                    , children:
                      [ R.a
                          { className: "navbar-item"
                          , children: [ R.text "Template" ]
                          , onClick: handler_ $ props.setPage TemplatePage
                          }
                      , R.a
                          { className: "navbar-item"
                          , children: [ R.text "Settings" ]
                          , onClick: handler_ $ props.setPage SettingsPage
                          }
                      , R.a
                          { className: "navbar-item"
                          , children: [ R.text "RecentlyChanged" ]
                          , onClick: handler_ $ props.setPage RecentlyChangedPage
                          }
                      , R.a
                          { className: "navbar-item"
                          , children: [ R.text "Logout" ]
                          , onClick: handler_ do
                              removeTokenFromLocalStorage
                              props.setPage LoginPage
                          }
                      ]
                    }
                ]
              }
          ]
        }
