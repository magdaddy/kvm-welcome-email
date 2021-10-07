module KvmMail.App.TemplatePage where

import ThisPrelude

import Control.Monad.Except (runExceptT)
import Data.Lens (set, view)
import Data.Maybe (fromMaybe)
import Data.Tuple.Nested ((/\))
import Effect.Aff (launchAff_)
import Effect.Exception (throw)
import Network.RemoteData (RemoteData(..), fromEither)
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler, handler_)
import React.Basic.Hooks (Component, component, useEffectOnce, useState, useState')
import React.Basic.Hooks as React
import KvmMail.App.Api.Backend as Api
import KvmMail.App.Data (AppError(..), Page(..))
import KvmMail.App.TestMail (mkTestMailComponent)
import KvmMail.Shared.Entry (Entry)
import KvmMail.Shared.Marked (markedS)
import KvmMail.Shared.Template (EmailTemplate, _EmailTemplateBody, _EmailTemplateSubject, expand)


mkTemplatePage :: Component { setPage :: Page -> Effect Unit, defaultEntry :: Entry }
mkTemplatePage = do
  templateEditor <- mkTemplateEditor
  testMailComponent <- mkTestMailComponent
  component "TemplatePage" \props -> React.do
    (edit :: Boolean) /\ setEdit <- useState' false
    (template :: RemoteData AppError EmailTemplate) /\ setTemplate <- useState' NotAsked

    let
      editTemplateHandler = do
        setEdit true

      cancelEditTemplateHandler = do
        setEdit false

      saveEditTemplateHandler tmpl = launchAff_ do
        result <- runExceptT $ Api.saveTemplate tmpl
        case result of
          Left err -> liftEffect $ throw $ show err
          Right _ -> do
            liftEffect $ setTemplate $ Success tmpl
            liftEffect $ setEdit false

      renderEmailMdShow emailT entry =
        let email = expand entry emailT
        in
        R.div_
          [ R.div
              { className: "has-text-right"
              , children:
                [ R.button
                    { className: "button is-primary"
                    , children: [ R.text "Edit" ]
                    , onClick: handler_ editTemplateHandler
                    }
                ]
              }
          , R.div
              { className: "m-2"
              , children:
                [ R.p_ [ R.strong_ [ R.text "Betreff: "], R.text $ email.subject ]
                ]
              }
          , R.hr {}
          , R.div
              { className: "m-2 content"
              , dangerouslySetInnerHTML: { __html: markedS email.body }
              }
          , R.hr {}
          , testMailComponent {}
          ]

    useEffectOnce do
      launchAff_ do
        liftEffect $ setTemplate Loading
        result <- runExceptT Api.getTemplate
        case result of
          Left (Unauthorized err) -> do
            liftEffect $ log $ "Unauthorized: " <> err
            liftEffect $ props.setPage LoginPage
          _ -> liftEffect $ setTemplate $ fromEither result
      pure mempty
    pure $
      R.div
        { className: "page template container is-max-desktop mb-5"
        , children:
          [ R.h1
              { className: "has-text-right has-text-white mr-5"
              , children: [ R.text "Template" ]
              }
          , R.div
              { className: "box"
              , children:
                [ case template of
                    NotAsked -> R.text "Not asked"
                    Loading -> R.text "Loading..."
                    Failure e -> R.text $ show e
                    Success email -> case edit of
                      true -> templateEditor
                        { template: email
                        , cancelHandler: cancelEditTemplateHandler
                        , saveHandler: saveEditTemplateHandler
                        }
                      false -> renderEmailMdShow email props.defaultEntry
                ]
              }
          ]
        }



mkTemplateEditor :: Component
  { template :: EmailTemplate
  , cancelHandler :: Effect Unit
  , saveHandler :: EmailTemplate -> Effect Unit
  }
mkTemplateEditor = do
  component "TemplatePage" \props -> React.do
    (templateEdited :: EmailTemplate) /\ setTemplateEdited <- useState $ props.template
    let
      field label input =
        R.div
          { className: "field"
          , children:
            [ R.label { className: "label", children: [ R.text label ] }
            , R.div { className: "control", children: [ input ] }
            ]
          }
    pure $
      R.div_
        [ R.div
            { className: "has-text-right"
            , children:
              [ R.button
                  { className: "button is-light"
                  , children: [ R.text "Cancel" ]
                  , onClick: handler_ props.cancelHandler
                  }
              ]
            }
        , field "Subject" $ R.input
            { className: "input"
            , value: view _EmailTemplateSubject templateEdited
            , onChange: handler targetValue \val ->
                -- setTemplateEdited (\(EmailTemplate e) -> EmailTemplate e { subject = fromMaybe "" val })
                setTemplateEdited \tE -> set (_EmailTemplateSubject) (fromMaybe "" val) tE
            }
        , field "Body" $ R.textarea
            { className: "textarea"
            , rows: 15
            , value: view _EmailTemplateBody templateEdited
            , onChange: handler targetValue \val ->
                setTemplateEdited \tE -> set (_EmailTemplateBody) (fromMaybe "" val) tE
            }
        , R.button
            { className: "button is-primary"
            , children: [ R.text "Save" ]
            , onClick: handler_ $ props.saveHandler templateEdited
            }
        ]

