
module Parser
    ( parseText
    , makeSentences
    ) where

import Types
import Tokenizer (makeTokens, fixSpaces)

-- розбиваємо токени на речення (кінець речення по крапці, знаку питання, оклику)
makeSentences :: [Token] -> [Sentence]
makeSentences [] = []
makeSentences tokens = 
  let (currentSentence, restTokens) = takeSentence tokens
  in if null currentSentence
     then makeSentences restTokens
     else currentSentence : makeSentences restTokens
  where
    takeSentence [] = ([], [])
    takeSentence (token:rest) = 
      case token of
        Punctuation c | c `elem` ".!?" -> ([token], rest)
        _ -> let (sent, remaining) = takeSentence rest
             in (token:sent, remaining)

-- основна функція парсингу тексту: розбиття на речення та токени
parseText :: String -> Text
parseText text = 
  let fixed = fixSpaces text
      tokens = makeTokens fixed
      sentences = makeSentences tokens
  in sentences