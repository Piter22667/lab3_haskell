
module TextFormatter
    ( sortText
    , getAllWords
    , countLetter
    , sortWords 
    ) where

import Data.Char (toLower)
import Data.List (sortBy)
import Types (Token(..), Sentence, Text)

-- отримуємо всі слова з тексту
getAllWords :: Text -> [String]
getAllWords allSentences = 
  concat [getWordsFromSentence sent | sent <- allSentences]
  where
    getWordsFromSentence sentence = [w | Word w <- sentence]

-- обрааховуємо скільки разів літера зустрічається в слові
countLetter :: Char -> String -> Int
countLetter letter word = 
  length [c | c <- word, toLower c == toLower letter]

-- сортуємо слова спочатку по кількості літери, потім по алфавіту
sortWords :: Char -> [String] -> [String]
sortWords letter words = sortBy compareWords words
  where
    compareWords word1 word2 = 
      let count1 = countLetter letter word1
          count2 = countLetter letter word2
      in if count1 == count2
         then compare word1 word2  -- якщо однакова кількість - по алфавіту
         else compare count1 count2  -- інакше - по кількості

-- отримуємо текст і літеру, повертає відсортовані слова
sortText :: Text -> Char -> [String]
sortText text letter = 
  let allWords = getAllWords text
      sortedWords = sortWords letter allWords
  in sortedWords