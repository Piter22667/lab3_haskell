
import System.IO
import Parser (parseText)
import TextFormatter (sortText)

main :: IO ()
main = do

  putStrLn "файл book.txt:\n"
  handle <- openFile "book.txt" ReadMode
  hSetEncoding handle utf8
  fileContent <- hGetContents handle
  
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