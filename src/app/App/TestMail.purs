module WelcomeEmail.App.TestMail where

import Prelude

import Data.Const (Const)
import Data.Either (Either(..))
import Data.Int (fromString) as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Effect.Aff.Class (class MonadAff)
import Formless (Validation, hoistFnE_)
import Formless as F
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..))
import Type.Proxy (Proxy(..))
import WelcomeEmail.App.Caps (class ManageSettings, class SendTestMail, sendTestMail)
import WelcomeEmail.App.Util (cls)
import WelcomeEmail.Shared.Boundary (TestMailPayload)


newtype EmailAddrForm (r :: Row Type -> Type) f = EmailAddrForm (r
  ( emailAddr :: f Void String String
  ))
derive instance newtypeEmailAddrForm :: Newtype (EmailAddrForm r f) _

-- type State = { result :: RemoteData String Unit }

data Action
  = Send TestMailPayload

component :: forall q m.
  MonadAff m =>
  ManageSettings m =>
  SendTestMail m =>
  H.Component q Unit Void m
component = H.mkComponent
  { initialState: \_ -> { result: NotAsked }
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Nothing }
  }
  where
  handleAction = case _ of
    Send pl -> do
      H.modify_ _ { result = Loading }
      result <- sendTestMail pl
      case result of
        Left err -> H.modify_ _ { result = Failure $ show err }
        Right response -> case response.error of
          Nothing -> H.modify_ _ { result = Success unit }
          Just err -> H.modify_ _ { result = Failure $ show err }

  render state = HH.div_
    [ HH.slot F._formless unit formComponent { emailAddr: "" } Send
    , case state.result of
        NotAsked -> HH.text ""
        Loading -> HH.text "Sending test mail..."
        Failure err -> HH.text err
        Success _ -> HH.text "Test mail sent successfully."
    ]

  formComponent :: F.Component EmailAddrForm (Const Void) () TestMailPayload TestMailPayload m
  formComponent = F.component formInput $ F.defaultSpec { render = renderFormless, handleEvent = F.raiseResult }
    where
    formInput inp =
      { validators: EmailAddrForm
        { emailAddr: F.noValidation
        }
      , initialInputs: Just (F.wrapInputFields inp)
      }

    renderFormless st =
      HH.div [ cls "field has-addons" ]
        [ HH.p [ cls "control" ]
            [ HH.button
                [ cls "button is-primary", HE.onClick \_ -> F.submit ]
                [ HH.text "Send test mail to" ]
            ]
        , HH.p [ cls "control is-expanded" ]
            [ HH.input
                [ cls "input", HP.value $ F.getInput _emailAddr st.form
                , HE.onValueInput (F.set _emailAddr)
                ]
            ]
        ]
      where
      _emailAddr = Proxy :: _ "emailAddr"

strIsInt :: ∀ form m. Monad m => Validation form m String String Int
strIsInt = hoistFnE_ $ \str -> maybe (Left $ "InvalidInt" <> str) Right (Int.fromString str)

