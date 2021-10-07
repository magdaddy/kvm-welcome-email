module KvmMail.App.Api.Web where

import ThisPrelude

import Data.Either (hush)
import Simple.JSON (read)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, removeItem, setItem)
import KvmMail.App.Util (jwtDecode)

tokenKey = "token" :: String

type Jwt = { exp :: Number, iat :: Number, sub :: String }
-- type Jwt = { exp :: JSDate, iat :: JSDate, sub :: String }

saveTokenToLocalStorage :: String -> Effect Unit
saveTokenToLocalStorage token = do
  win <- window
  storage <- localStorage win
  setItem tokenKey token storage

getTokenFromLocalStorage :: Effect (Maybe String)
getTokenFromLocalStorage = do
  win <- window
  storage <- localStorage win
  getItem tokenKey storage

removeTokenFromLocalStorage :: Effect Unit
removeTokenFromLocalStorage = do
  win <- window
  storage <- localStorage win
  removeItem tokenKey storage

getJwtFromLocalStorage :: Effect (Maybe Jwt)
getJwtFromLocalStorage = do
  mbToken <- getTokenFromLocalStorage
  let (mbRawJwt :: Maybe Jwt) = mbToken >>= (hush <<< read <<< jwtDecode)
  pure mbRawJwt
