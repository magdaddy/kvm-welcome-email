module WelcomeEmail.App.Util where

import Foreign (Foreign)


foreign import jwtDecode :: String -> Foreign
foreign import jwtDecodeHeader :: String -> Foreign

