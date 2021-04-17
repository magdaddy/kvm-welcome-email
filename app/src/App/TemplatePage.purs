module WelcomeEmail.App.TemplatePage where

import Prelude

import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..))
import Type.Proxy (Proxy(..))
import WelcomeEmail.App.Component.RawHTML as RawHTML
import WelcomeEmail.App.Data (Action(..), Slots, TemplatePageState)
import WelcomeEmail.App.Env (DefaultEntry)
import WelcomeEmail.Shared.Template (EmailTemplate(..), expand)

type State = TemplatePageState

render :: forall m. MonadEffect m => State -> DefaultEntry -> H.ComponentHTML Action Slots m
render state defaultEntry =
  HH.div
    [ HP.classes [ HH.ClassName "template" ] ]
    [ HH.h1_ [ HH.text "Template" ]
    , case state.edit of
        true -> HH.div_
          [ HH.button
            [ HE.onClick \_ -> CancelEditTemplateClicked ]
            [ HH.text "Cancel" ]
          , HH.button
            [ HE.onClick \_ -> SaveTemplateClicked ]
            [ HH.text "Save" ]
          ]
        false -> HH.button
          [ HE.onClick \_ -> EditTemplateClicked ]
          [ HH.text "Edit" ]
    , case state.template of
        NotAsked -> HH.text "Not asked"
        Loading -> HH.text "Loading..."
        Failure e -> HH.text $ show e
        Success email -> case state.edit of
          true -> renderEmailEdit email
          false -> renderEmailMdShow email defaultEntry
    -- , HH.div_
    --     [ HH.text $ show defaultEntry ]
    ]

renderEmailEdit :: forall cs m. EmailTemplate -> H.ComponentHTML Action cs m
renderEmailEdit (EmailTemplate email) =
  -- let
  --   subject = email # head
  --   body = email # tail # map (joinWith "<br>")
  -- in
  HH.div_
    [ HH.input
      [ HP.value email.subject
      , HE.onValueChange \newVal -> TemplateSubjectEdited newVal
      ]
    , HH.textarea
      [ HP.value email.body
      , HE.onValueChange \newVal -> TemplateBodyEdited newVal
      ]
    ]

renderEmailMdShow :: forall m. MonadEffect m => EmailTemplate -> DefaultEntry -> H.ComponentHTML Action Slots m
renderEmailMdShow emailT entry =
  let email = expand entry emailT
  in
  HH.div_
    [ HH.p_ [ HH.strong_ [ HH.text "Betreff: "], HH.text $ email.subject ]
    , HH.slot_ (Proxy :: _ "rawHtml") 0 RawHTML.component { markdown: email.body }
    ]
