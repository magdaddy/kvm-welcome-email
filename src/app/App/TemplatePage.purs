module WelcomeEmail.App.TemplatePage where

import Prelude

import Data.Either (Either(..))
import Data.Lens (set, view)
import Data.Maybe (fromMaybe)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Network.RemoteData (RemoteData(..), fromEither)
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler, handler_)
import React.Basic.Hooks (Component, component, useEffectOnce, useState, useState')
import React.Basic.Hooks as React
import WelcomeEmail.App.Api as Api
import WelcomeEmail.App.Data (AppError)
import WelcomeEmail.App.TestMail (mkTestMailComponent)
import WelcomeEmail.Shared.Entry (Entry)
import WelcomeEmail.Shared.Marked (markedS)
import WelcomeEmail.Shared.Template (EmailTemplate, _EmailTemplateBody, _EmailTemplateSubject, expand)


mkTemplatePage :: Component { defaultEntry :: Entry }
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
        result <- Api.saveTemplate tmpl
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
        result <- Api.getTemplate
        liftEffect $ setTemplate $ fromEither result
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



-- render :: forall m.
--   MonadAff m =>
--   ManageSettings m =>
--   SendTestMail m =>
--   State -> Entry -> H.ComponentHTML Action Slots m
-- render state defaultEntry =
--   HH.div [ cls "template container is-max-desktop mb-5" ]
--     [ HH.h1 [ cls "has-text-right has-text-white mr-5" ] [ HH.text "Template" ]
--     , HH.div [ cls "box" ]
--         [ case state.template of
--             NotAsked -> HH.text "Not asked"
--             Loading -> HH.text "Loading..."
--             Failure e -> HH.text $ show e
--             Success email -> case state.edit of
--               true -> renderEmailEdit' email
--               false -> renderEmailMdShow' email defaultEntry
--         ]
--     ]

-- renderEmailEdit' :: forall cs m. EmailTemplate -> H.ComponentHTML Action cs m
-- renderEmailEdit' (EmailTemplate email) =
--   HH.div [ cls "" ]
--     [ HH.div [ cls "has-text-right" ]
--         [ HH.button
--             [ cls "button is-light", HE.onClick \_ -> CancelEditTemplateClicked ]
--             [ HH.text "Cancel" ]
--         ]
--     , field "Subject" $
--       HH.input
--         [ cls "input", HP.value email.subject
--         , HE.onValueChange \newVal -> TemplateSubjectEdited newVal
--         ]
--     , field "Body" $
--       HH.textarea
--         [ cls "textarea", HP.rows 15, HP.value email.body
--         , HE.onValueChange \newVal -> TemplateBodyEdited newVal
--         ]
--     , HH.button
--         [ cls "button is-primary", HE.onClick \_ -> SaveTemplateClicked ]
--         [ HH.text "Save" ]
--     ]
--   where
--   field label input = HH.div [ cls "field" ]
--     [ HH.label [ cls "label" ] [ HH.text label ]
--     , HH.div [ cls "control" ] [ input ]
--     ]


-- renderEmailMdShow' :: forall m.
--   MonadAff m =>
--   ManageSettings m =>
--   SendTestMail m =>
--   EmailTemplate -> Entry -> H.ComponentHTML Action Slots m
-- renderEmailMdShow' emailT entry =
--   let email = expand entry emailT
--   in
--   HH.div [ cls "" ]
--     [ HH.div [ cls "has-text-right" ]
--         [ HH.button
--             [ cls "button is-primary", HE.onClick \_ -> EditTemplateClicked ]
--             [ HH.text "Edit" ]
--         ]
--     , HH.div [ cls "m-2"
--               -- , HP.style "border: 1px solid black;"
--               ]
--         [ HH.p_ [ HH.strong_ [ HH.text "Betreff: "], HH.text $ email.subject ] ]
--     , HH.hr_
--     , HH.div [ cls "m-2" ]
--         [ HH.slot_ (Proxy :: _ "rawHtml") 0 RawHTML.component { markdown: email.body } ]
--     , HH.hr_
--     , HH.slot_ (Proxy :: _ "testMail") 0 TestMail.component unit
--     ]
