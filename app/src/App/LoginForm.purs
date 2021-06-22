module WelcomeEmail.App.LoginForm where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect.Aff.Class (class MonadAff)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..))
import Type.Proxy (Proxy(..))
import WelcomeEmail.App.Util (cls)
import WelcomeEmail.Shared.Boundary (LoginData)

newtype LoginForm (r :: Row Type -> Type) f = LoginForm (r
  ( username :: f Void String String
  , password :: f Void String String
  ))
derive instance newtypeLoginForm :: Newtype (LoginForm r f) _

data Action
  = Login LoginData

component :: forall q m.
  MonadAff m =>
  H.Component q Unit Void m
component = H.mkComponent
  { initialState: \_ -> { result: NotAsked }
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Nothing }
  }
  where
  handleAction = case _ of
    Login loginData -> do
      mempty

  render state = HH.div_
    [ HH.slot F._formless unit formComponent { username: "", password: "" } Login
    , case state.result of
        NotAsked -> HH.text ""
        Loading -> HH.text "Sending test mail..."
        Failure err -> HH.text err
        Success _ -> HH.text "Test mail sent successfully."
    ]

  formComponent :: F.Component LoginForm (Const Void) () LoginData LoginData m
  formComponent = F.component formInput $ F.defaultSpec { render = renderFormless, handleEvent = F.raiseResult }
    where
    formInput inp =
      { validators: LoginForm
        { username: F.noValidation
        , password: F.noValidation
        }
      , initialInputs: Just (F.wrapInputFields inp)
      }

    renderFormless st =
      HH.div_
        [ field "Username: " $ HH.input
            [ cls "input", HP.value $ F.getInput _username st.form
            , HE.onValueInput (F.set _username)
            ]
        , field "Password: " $ HH.input
            [ cls "input", HP.value $ F.getInput _password st.form
            , HE.onValueInput (F.set _password)
            ]
        , HH.button
            [ cls "button is-primary", HE.onClick \_ -> F.submit ]
            [ HH.text "Login" ]
        ]
      where
      _username = Proxy :: _ "username"
      _password = Proxy :: _ "password"
      field label input = HH.div [ cls "field" ]
        [ HH.label [ cls "label" ] [ HH.text label ]
        , HH.div [ cls "control" ] [ input ]
        ]

