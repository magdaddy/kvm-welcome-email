module KvmMail.Shared.Marked
  ( RawHTML
  , marked
  , markedS
  ) where

newtype RawHTML = RawHTML String

foreign import markedImpl :: String -> String

marked :: String -> RawHTML
marked str = RawHTML (markedImpl str)

markedS :: String -> String
markedS = markedImpl
