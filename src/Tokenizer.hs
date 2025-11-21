module Tokenizer
    (makeTokens
    , fixSpaces
    ) where

import Data.Char (isAlphaNum)
import Types (Token(..))


-- замінюємо табуляції на один пробіл
fixSpaces :: String -> String
fixSpaces text = unwords (words text)

-- розбиваємо текст на токени
makeTokens :: String -> [Token]
makeTokens [] = []
makeTokens (firstChar:restChars)
  | isAlphaNum firstChar = 
      let (wordChars, afterWord) = span isAlphaNum (firstChar:restChars)
      in Word wordChars : makeTokens afterWord
  | firstChar `elem` ".,;:!?" = 
      Punctuation firstChar : makeTokens restChars
  | otherwise = 
      makeTokens restChars