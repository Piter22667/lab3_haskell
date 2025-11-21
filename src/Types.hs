module Types
    ( Token(..)
    , Sentence
    , Text
    ) where

data Token = Word String
        | Punctuation Char
        deriving (Show, Eq)


type Sentence = [Token]
type Text = [Sentence]