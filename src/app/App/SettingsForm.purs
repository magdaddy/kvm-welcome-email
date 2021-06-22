module WelcomeEmail.App.SettingsForm where

import Prelude

import Data.Const (Const)
import Data.Either (Either(..))
import Data.Int (fromString) as Int
import Data.Lens (Lens', preview, set)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.String (null)
import Effect.Aff.Class (class MonadAff)
import Effect.Console (logShow)
import Formless (Validation, hoistFnE_)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..), _Success, fromEither)
import Record (modify)
import Type.Proxy (Proxy(..))
import WelcomeEmail.App.Caps (class ManageSettings, class SendTestMail, getSettings, saveSettings)
import WelcomeEmail.App.Data (AppError)
import WelcomeEmail.App.TestMail as TestMail
import WelcomeEmail.App.Util (cls)
import WelcomeEmail.Shared.Boundary (Settings)



type LocalSettings =
  { senderAddress :: String
  , username :: String
  , password :: String
  , host :: String
  , port :: Int
  }

newtype ContactForm (r :: Row Type -> Type) f = ContactForm (r
  ( senderAddress :: f Void String String
  , username :: f Void String String
  , password :: f Void String String
  , host :: f Void String String
  , port :: f String String Int
  ))
derive instance newtypeContactForm :: Newtype (ContactForm r f) _

data Action
  = Submit LocalSettings
  | GetSettings
  | EditClicked
  | CancelClicked


component :: forall q m.
  MonadAff m =>
  ManageSettings m =>
  SendTestMail m =>
  H.Component q Unit Void m
component = H.mkComponent
  { initialState: \_ -> { edit: false, settings: (NotAsked :: RemoteData AppError Settings) }
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Just GetSettings }
  }
  where
  handleAction = case _ of
    Submit ls -> do
      H.liftEffect $ logShow ls
      st <- H.get
      case preview (_settings <<< _Success) st of
        Nothing -> H.liftEffect $ logShow "This can't happen"
        Just s -> do
          let ss = updWithLocal ls s
          H.modify_ \st' -> set (_settings <<< _Success) ss st'
          result <- saveSettings ss
          case result of
            Left err -> H.liftEffect $ logShow err
            Right _ ->  H.modify_ _ { edit = false }
    GetSettings -> do
      H.modify_ _ { settings = Loading }
      response <- getSettings
      H.modify_ _ { settings = (fromEither response) }
    EditClicked -> do
      H.modify_ _ { edit = true }
    CancelClicked -> do
      H.modify_ _ { edit = false }

  render state =
    HH.section [ cls "box" ]
      [ --HH.h1_ [ HH.text "Mailer" ]
        if state.edit then
          case state.settings of
            Success settings -> HH.div_
              [ HH.div [ cls "has-text-right" ]
                  [ HH.button
                      [ cls "button is-light", HE.onClick \_ -> CancelClicked ]
                      [ HH.text "Cancel" ]
                  ]
              , HH.slot F._formless unit formComponent (toLocal settings) Submit
              ]

            _ -> HH.text "This should never happen"
        else
          case state.settings of
            NotAsked -> HH.text "not asked"
            Loading -> HH.text "loading"
            Failure err -> HH.text $ show err
            Success settings -> HH.div [ cls "" ]
              [ HH.div [ cls "has-text-right"]
                  [ HH.button
                      [ cls "button is-primary", HE.onClick \_ -> EditClicked ]
                      [ HH.text "Edit" ]
                  ]
              -- , HH.text $ show settings
              , field "Sender address" $ HH.text $ showEmpty settings.senderAddress
              , field "Username" $ HH.text $ showEmpty settings.nodeMailer.auth.user
              , field "Password" $ HH.text $ showEmpty settings.nodeMailer.auth.pass
              , field "Host" $ HH.text $ showEmpty settings.nodeMailer.host
              , field "Port" $ HH.text $ show settings.nodeMailer.port
              , HH.hr_
              , HH.slot_ (Proxy :: _ "testMail") 0 TestMail.component unit
              ]
      ]
    where
    field label input = HH.div [ cls "field" ]
      [ HH.label [ cls "label" ] [ HH.text label ]
      , HH.div [ cls "control" ] [ input ]
      ]
    showEmpty str = if null str then "---" else str


  formComponent :: F.Component ContactForm (Const Void) () LocalSettings LocalSettings m
  formComponent = F.component formInput $ F.defaultSpec { render = renderFormless, handleEvent = F.raiseResult }
    where
    formInput inp =
      { validators: ContactForm
        { senderAddress: F.noValidation
        , username: F.noValidation
        , password: F.noValidation
        , host: F.noValidation
        , port: strIsInt
        }
      , initialInputs: Just (F.wrapInputFields (modify (Proxy :: _ "port") show inp))
      }

    renderFormless st =
      HH.div_ --formContent_
        [ field "Sender address: " $ HH.input
            [ cls "input", HP.value $ F.getInput _senderAddress st.form
            , HE.onValueInput (F.set _senderAddress)
            ]
        , field "Username: " $ HH.input
            [ cls "input", HP.value $ F.getInput _username st.form
            , HE.onValueInput (F.set _username)
            ]
        , field "Password: " $ HH.input
            [ cls "input", HP.value $ F.getInput _password st.form
            , HE.onValueInput (F.set _password)
            ]
        , field "Host: " $ HH.input
            [ cls "input", HP.value $ F.getInput _host st.form
            , HE.onValueInput (F.set _host)
            ]
        , field "Port: " $ HH.input
            [ cls "input", HP.value $ F.getInput _port st.form
            , HE.onValueInput (F.set _port)
            ]
        , HH.button
            [ cls "button is-primary", HE.onClick \_ -> F.submit ]
            [ HH.text "Save" ]
        ]
      where
      _senderAddress = Proxy :: _ "senderAddress"
      _username = Proxy :: _ "username"
      _password = Proxy :: _ "password"
      _host = Proxy :: _ "host"
      _port = Proxy :: _ "port"
      field label input = HH.div [ cls "field" ]
        [ HH.label [ cls "label" ] [ HH.text label ]
        , HH.div [ cls "control" ] [ input ]
        ]

_settings :: forall a r. Lens' { settings :: a | r } a
_settings = prop (Proxy :: _ "settings")

strIsInt :: ∀ form m. Monad m => Validation form m String String Int
strIsInt = hoistFnE_ $ \str -> maybe (Left $ "InvalidInt" <> str) Right (Int.fromString str)

toLocal :: Settings -> LocalSettings
toLocal s =
  { senderAddress: s.senderAddress
  , username: s.nodeMailer.auth.user
  , password: s.nodeMailer.auth.pass
  , host: s.nodeMailer.host
  , port: s.nodeMailer.port
  }

updWithLocal :: LocalSettings -> Settings -> Settings
updWithLocal ls s = s { nodeMailer = nm, senderAddress = ls.senderAddress }
  where
  nm = s.nodeMailer
    { auth = { user: ls.username, pass: ls.password }
    , host = ls.host
    , port = ls.port
    }

