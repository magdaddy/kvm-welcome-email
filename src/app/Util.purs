module WelcomeEmail.App.Util where

import Foreign (Foreign)


foreign import jwtDecode :: String -> Foreign
foreign import jwtDecodeHeader :: String -> Foreign

-- import Halogen.HTML as HH
-- import Halogen.HTML.Properties as HP

-- cls :: forall r t. String -> HH.IProp ( "class" :: String | r ) t
-- cls = HP.class_ <<< HH.ClassName
