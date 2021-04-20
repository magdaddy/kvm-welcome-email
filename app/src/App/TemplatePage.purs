module WelcomeEmail.App.TemplatePage where

import Prelude

import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..))
import Type.Proxy (Proxy(..))
import WelcomeEmail.App.Caps (class ManageSettings, class SendTestMail)
import WelcomeEmail.App.Component.RawHTML as RawHTML
import WelcomeEmail.App.Data (Action(..), Slots, TemplatePageState)
import WelcomeEmail.App.TestMail as TestMail
import WelcomeEmail.App.Util (cls)
import WelcomeEmail.Shared.Entry (Entry)
import WelcomeEmail.Shared.Template (EmailTemplate(..), expand)

type State = TemplatePageState

render :: forall m.
  MonadAff m =>
  ManageSettings m =>
  SendTestMail m =>
  State -> Entry -> H.ComponentHTML Action Slots m
render state defaultEntry =
  HH.div [ cls "template container is-max-desktop mb-5" ]
    [ HH.h1 [ cls "has-text-right has-text-white mr-5" ] [ HH.text "Template" ]
    , HH.div [ cls "box" ]
        [ case state.template of
            NotAsked -> HH.text "Not asked"
            Loading -> HH.text "Loading..."
            Failure e -> HH.text $ show e
            Success email -> case state.edit of
              true -> renderEmailEdit email
              false -> renderEmailMdShow email defaultEntry
        ]
    ]

renderEmailEdit :: forall cs m. EmailTemplate -> H.ComponentHTML Action cs m
renderEmailEdit (EmailTemplate email) =
  HH.div [ cls "" ]
    [ HH.div [ cls "has-text-right" ]
        [ HH.button
            [ cls "button is-light", HE.onClick \_ -> CancelEditTemplateClicked ]
            [ HH.text "Cancel" ]
        ]
    , field "Subject" $
      HH.input
        [ cls "input", HP.value email.subject
        , HE.onValueChange \newVal -> TemplateSubjectEdited newVal
        ]
    , field "Body" $
      HH.textarea
        [ cls "textarea", HP.rows 15, HP.value email.body
        , HE.onValueChange \newVal -> TemplateBodyEdited newVal
        ]
    , HH.button
        [ cls "button is-primary", HE.onClick \_ -> SaveTemplateClicked ]
        [ HH.text "Save" ]
    ]
  where
  field label input = HH.div [ cls "field" ]
    [ HH.label [ cls "label" ] [ HH.text label ]
    , HH.div [ cls "control" ] [ input ]
    ]


renderEmailMdShow :: forall m.
  MonadAff m =>
  ManageSettings m =>
  SendTestMail m =>
  EmailTemplate -> Entry -> H.ComponentHTML Action Slots m
renderEmailMdShow emailT entry =
  let email = expand entry emailT
  in
  HH.div [ cls "" ]
    [ HH.div [ cls "has-text-right" ]
        [ HH.button
            [ cls "button is-primary", HE.onClick \_ -> EditTemplateClicked ]
            [ HH.text "Edit" ]
        ]
    , HH.div [ cls "m-2"
              -- , HP.style "border: 1px solid black;"
              ]
        [ HH.p_ [ HH.strong_ [ HH.text "Betreff: "], HH.text $ email.subject ] ]
    , HH.hr_
    , HH.div [ cls "m-2" ]
        [ HH.slot_ (Proxy :: _ "rawHtml") 0 RawHTML.component { markdown: email.body } ]
    , HH.hr_
    , HH.slot_ (Proxy :: _ "testMail") 0 TestMail.component unit
    ]
