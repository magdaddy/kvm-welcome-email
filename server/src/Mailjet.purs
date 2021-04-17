module Mailjet where

import Prelude

import Affjax as AX
import Affjax.RequestBody (json)
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut (encodeJson, stringify)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.MediaType.Common (applicationJSON)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, message)
import Effect.Class (liftEffect)
import Effect.Console (log)


basicAuthHeader :: RequestHeader
basicAuthHeader = RequestHeader "Authorization" "Basic YWQ1N2VmYmFlZDgzYWQxZDhlMWY0NGRmN2Q5N2FkMDA6MDdhNDAyNjM1MTAxNmM3MWUzZjNkOTczMjE4ODRmZGI="

launch :: forall a. Aff a -> Effect Unit
launch = launchAff_


send :: Aff Unit
send = do
  log url # liftEffect
--   result <- AX.post ResponseFormat.string url Nothing
  result <- AX.request req
  liftEffect $ void $ case result of
    Left err ->  ("GET failed: " <> (AX.printError err)) # log
    Right response -> response # {-stringify #-} show # log -- # decodeEntries # lmap show # log
  where
  url = "https://api.mailjet.com/v3.1/send"
  req = AX.defaultRequest {
      method = Left POST,
      url = url,
      headers = [ContentType applicationJSON, basicAuthHeader],
      content = Just reqBody,
      responseFormat = ResponseFormat.string
      }
  reqBody = json $ encodeJson mail


mail =
  { "Messages": [
    {
        "From": {
          "Email": "magnus.herold@gmail.com",
          "Name": "Magnus Gmail"
        },
        "To": [{
          "Email": "magnus.herold@slowtec.de",
          "Name": "Magnus Slowtec"
        }],
        "Subject": "Test1",
        "TextPart": "Yo, this is test 1."
    }
    ] }
