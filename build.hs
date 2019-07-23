{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
import Data.Text hiding (init,last)
import Text.ParserCombinators.Parsec 
import System.IO.Strict as S
import System.IO

-- | Page Data type
data Page = Page [Element]

instance Show Page where show = showPage

showPage :: Page -> String
showPage (Page elems) = Prelude.concat $  Prelude.map (\x -> show x ++ "\n\n") elems

-- | Element datatype
data Element = P Text
  | Export {lang :: Maybe Text, contents :: Text}
  | Src {lang :: Maybe Text, contents :: Text}
  | Example {contents :: Text}
  | Link {body :: Text, url :: Text}
  deriving (Show)



-- | parse a given file
parseFile :: String -> IO Page
parseFile file = do
  h <- openFile file ReadMode
  hSetEncoding h latin1
  content <- S.hGetContents h
  case parse fileParser "tasks" content of
    Left err -> do
      print err
      pure $ Page $[ P "hello"]
    Right val -> pure val

parseExportLang :: Parser Element
parseExportLang = do
  _ <- string "#+BEGIN_EXPORT "
  exportLang <- Just <$> pack <$>  many1 (noneOf "\n")
  srcContents <- pack <$> manyTill anyChar (try  (string "#+END_EXPORT\n"))
  return $   Export exportLang srcContents 

parseExportNoLang :: Parser Element
parseExportNoLang = do
  _ <- string "#+BEGIN_EXPORT"
  exportText <- pack <$> manyTill anyChar (try  (string "#+END_EXPORT\n"))
  return $ Export Nothing exportText

parseExport :: Parser Element
parseExport = try parseExportLang <|> parseExportNoLang


parseLink :: Parser Element
parseLink = do
  _ <- string "[["
  linkText <- pack <$> manyTill anyChar (try (string "]["))
  linkUrl  <- pack <$> manyTill anyChar (try (string "]]"))
  return $ Link linkText linkUrl

parseSrcLang :: Parser Element
parseSrcLang = do
  _ <- string "#+BEGIN_SRC "
  exportLang <- Just <$> pack <$> many1 (noneOf "\n")
  srcText <- pack <$> manyTill anyChar (try  (string "#+END_SRC\n"))
  return $ Export exportLang srcText

parseSrcNoLang :: Parser Element
parseSrcNoLang = do
  _ <- string "#+BEGIN_SRC"
  srcText <- pack <$>  manyTill anyChar (try  (string "#+END_SRC\n"))
  return $ Export Nothing srcText
  
parseSrc :: Parser Element
parseSrc = try parseSrcLang <|> parseSrcNoLang


parseExample :: Parser Element
parseExample = do
  _ <- string "#+BEGIN_EXAMPLE"
  exampleText <- pack <$>  manyTill anyChar (try  (string "#+END_EXAMPLE\n"))
  return $ Example exampleText

parseTextMid :: Parser Element
parseTextMid = do
  text <- manyTill anyChar ( lookAhead end)  :: Parser String
  return $ P (pack text)

parseTextEnd  :: Parser Element
parseTextEnd = do
  text <- many1 anyChar
  return $ P (pack text)

parseText :: Parser Element
parseText = try parseTextMid <|> parseTextEnd

-- | used in text parser texts until we've reached eof or start of other token
end :: Parser String
end = try $ string "[["
  <|> try ( string "#+BEGIN_EXAMPLE")
  <|> try (string "#+BEGIN_EXPORT")
  <|> string "#+BEGIN_SRC"

  
parseElem :: Parser Element
parseElem = try parseLink
  <|> try parseExport
  <|> try parseExample
  <|> try parseSrc
  <|> parseText
  
fileParser :: Parser Page
fileParser =  Page <$>  many1 parseElem

main :: IO ()
main = parseFile "test.org" >>= print
