import Data.Char (isAlphaNum, toLower)
import Data.List (sortBy)
import System.IO


data Token = Word String | Punctuation Char
  deriving (Show, Eq)

type Sentence = [Token]
type Text = [Sentence]


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



main :: IO ()
main = do

  putStrLn "файл book.txt:\n"
  handle <- openFile "book.txt" ReadMode
  hSetEncoding handle utf8
  fileContent <- hGetContents handle
  
  -- Парсимо текст
  let parsedText = parseText fileContent
  putStrLn (show parsedText)
  
  putStrLn "Введіть літеру:"
  userInput <- getLine
  
  if null userInput
    then putStrLn "Ви не ввели літеру."
    else do
      let givenLetter = head userInput
      let result = sortText parsedText givenLetter
      
      putStrLn ("\nСлова відсортовані за літерою '" ++ [givenLetter] ++ "':")
      mapM_ putStrLn result
  
  hClose handle
