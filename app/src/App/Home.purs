module App.Home where

import Prelude

import Data.Either (Either(..))
import Data.Lens (Lens', preview, set, view)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Console (log)
import Effect.Exception (throw)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HPA
import Network.RemoteData (RemoteData(..), _Success, fromEither)
import Type.Proxy (Proxy(..))
import WelcomeEmail.App.Caps (class ManageSettings, class ManageTemplate, class SendTestMail, getSettings, getTemplate, saveTemplate, sendTestMail)
import WelcomeEmail.App.Data (Action(..), Page(..), Slots, State)
import WelcomeEmail.App.StatusPage as StatusPage
import WelcomeEmail.App.SettingsPage as SettingsPage
import WelcomeEmail.App.TemplatePage as TemplatePage
import WelcomeEmail.App.Util (cls)
import WelcomeEmail.Shared.Entry (Entry)
import WelcomeEmail.Shared.Template (EmailTemplate(..))

component :: forall r q o m.
  MonadEffect m =>
  MonadAff m =>
  ManageTemplate m =>
  ManageSettings m =>
  SendTestMail m =>
  H.Component q { defaultEntry :: Entry | r } o m
component = do
  H.mkComponent
    { initialState: \{ defaultEntry } ->
      { count: 0
      , defaultEntry: defaultEntry
      , settings: NotAsked
      , page: Status
      , templatePage:
        { edit: false
        , template: NotAsked
        , templateEdited: EmailTemplate { subject: "", body: "" }
        }
      }
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction, initialize = Just Initialize }
    }

-- View --
render :: forall m.
  MonadAff m =>
  ManageSettings m =>
  SendTestMail m =>
  State -> H.ComponentHTML Action Slots m
render state =
  HH.div
    [ HP.id "app" ]
    [ bulmaNavbar
    , HH.div [ cls "page" ]
        [ renderPage state ]
    ]

renderPage :: forall m.
  MonadAff m =>
  ManageSettings m =>
  SendTestMail m =>
  State -> H.ComponentHTML Action Slots m
renderPage state = case state.page of
  Status -> StatusPage.render state
  Template -> TemplatePage.render state.templatePage state.defaultEntry
  Settings -> SettingsPage.render state

oldNavbar :: forall m. H.ComponentHTML Action Slots m
oldNavbar =
  HH.div
    [ HP.classes [ HH.ClassName "header" ] ]
    [ HH.button
        [ HE.onClick \_ -> ShowPage Status ]
        [ HH.text "Status" ]
    , HH.button
        [ HE.onClick \_ -> ShowPage Template ]
        [ HH.text "Template" ]
    , HH.button
        [ HE.onClick \_ -> ShowPage Settings ]
        [ HH.text "Settings" ]
    ]

bulmaNavbar :: forall m. H.ComponentHTML Action Slots m
bulmaNavbar =
  HH.nav [ cls "navbar", HPA.role "navigation", HPA.label "main navigation" ]
    [ HH.div [ cls "navbar-brand" ]
        [ HH.a [ cls "navbar-item", HE.onClick \_ -> ShowPage Status ]
            [ HH.text "KVM Mail"]
        , navbarBurger
        ]
    , HH.div [ cls "navbar-menu" ]
        [ HH.div [ cls "navbar-start" ]
            [ HH.a
                [ cls "navbar-item", HE.onClick \_ -> ShowPage Template ]
                [ HH.text "Template"]
            , HH.a
                [ cls "navbar-item", HE.onClick \_ -> ShowPage Settings ]
                [ HH.text "Settings"]
            ]
        ]
    ]
  where
  navbarBurger =
    HH.a [ cls "navbar-burger", HPA.role "button", HPA.label "menu", HPA.expanded "false" ]
      [ HH.span [ HPA.hidden "true" ] []
      , HH.span [ HPA.hidden "true" ] []
      , HH.span [ HPA.hidden "true" ] []
      ]



-- Update --
handleAction :: forall cs o m. MonadEffect m =>
  ManageTemplate m =>
  ManageSettings m =>
  SendTestMail m =>
  Action → H.HalogenM State Action cs o m Unit
handleAction = case _ of
  Initialize -> do
    handleAction GetTemplate
    handleAction GetSettings
  ShowPage page -> H.modify_ _ { page = page }

  GetTemplate -> do
    H.modify_ \st -> set (_templatePage <<< __template) Loading st
    response <- getTemplate
    case response of
      Left err -> H.modify_ \st -> set (_templatePage <<< __template) (Failure err) st
      Right template -> H.modify_ \st -> set (_templatePage <<< __template) (Success template) st
  EditTemplateClicked -> do
    st <- H.get
    case preview (_templatePage <<< __template <<< _Success) st of
      Nothing -> liftEffect $ throw "No template"
      Just temp -> do
        H.modify_ \st' -> set (_templatePage <<< __templateEdited) temp st'
        H.modify_ \st' -> set (_templatePage <<< __edit) true st'
  SaveTemplateClicked -> do
    st <- H.get
    -- liftEffect $ log $ show st
    let t = view (_templatePage <<< __templateEdited) st
    liftEffect $ log $ show t
    response <- saveTemplate t
    case response of
      Left err -> liftEffect $ throw $ show err
      Right _ -> do
        H.modify_ \st' -> set (_templatePage <<< __template <<< _Success) t st'
        H.modify_ \st' -> set (_templatePage <<< __edit) false st'
  CancelEditTemplateClicked ->
    H.modify_ \st -> set (_templatePage <<< __edit) false st
  TemplateSubjectEdited newVal ->
    H.modify_ \st -> set (_templatePage <<< __templateEdited <<< _Newtype <<< __subject) newVal st
  TemplateBodyEdited newVal ->
    H.modify_ \st -> set (_templatePage <<< __templateEdited <<< _Newtype <<< __body) newVal st

  GetSettings -> do
    H.modify_ \st -> set (_settings) Loading st
    response <- getSettings
    H.modify_ \st -> set (_settings) (fromEither response) st

  SendTestMailClicked -> do
    result <- sendTestMail { emailAddr: "" }
    case result of
      Left err -> liftEffect $ log $ show err
      Right _ -> pure unit

_templatePage :: forall a r. Lens' { templatePage :: a | r } a
_templatePage = prop (Proxy :: _ "templatePage")

__template :: forall a r. Lens' { template :: a | r } a
__template = prop (Proxy :: _ "template")

__edit :: forall a r. Lens' { edit :: a | r } a
__edit = prop (Proxy :: _ "edit")

__templateEdited :: forall a r. Lens' { templateEdited :: a | r } a
__templateEdited = prop (Proxy :: _ "templateEdited")

__subject :: forall a r. Lens' { subject :: a | r } a
__subject = prop (Proxy :: _ "subject")
__body :: forall a r. Lens' { body :: a | r } a
__body = prop (Proxy :: _ "body")

_settings :: forall a r. Lens' { settings :: a | r } a
_settings = prop (Proxy :: _ "settings")
