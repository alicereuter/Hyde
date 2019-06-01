{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
import Network.HTTP.Conduit (simpleHttp)
import Data.Aeson
import Data.Text hiding (init,last)
import Text.ParserCombinators.Parsec (Parser
                                     ,parse
                                     ,(<|>)
                                     ,try
                                     ,choice
                                     ,many,
                                      many1
                                     ,endBy
                                     ,sepBy
                                     ,oneOf
                                     ,noneOf
                                     ,digit
                                     ,string
                                     ,letter
                                     ,notFollowedBy
                                     ,manyTill
                                     ,anyChar
                                     ,lookAhead
                                     ,eof
                                     ,char)
import qualified Data.ByteString.Lazy as B
import GHC.Generics
import System.IO.Strict as S
import System.Environment
import System.IO

-- | Page Data type
data Page = Page [Element]

instance Show Page where show = showPage

showPage :: Page -> String
showPage (Page elems) = Prelude.concat $  Prelude.map (\x -> show x ++ "\n") elems

-- | Element datatype
data Element = P Text
  | Export {lang :: Maybe Text, contents :: Text}
  | Src {lang :: Maybe Text, contents :: Text}
  | Example {contents :: Text}
  deriving (Show)


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


parseExportLang = do
  string "#+BEGIN_EXPORT "
  exportLang <- many1 (noneOf "\n")
  contents <- manyTill anyChar (try  (string "#+END_EXPORT\n"))
 -- _ <- string "\n"
  return $   Export (Just (pack exportLang)) (pack contents) 

parseExportNoLang = do
  string "#+BEGIN_EXPORT"
  contents <- manyTill anyChar (try  (string "#+END_EXPORT\n"))
 -- _ <- string "\n"
  return $   Export Nothing (pack contents) 

parseExport = try parseExportLang <|> parseExportNoLang

parseSrcLang = do
  string "#+BEGIN_SRC "
  exportLang <- many1 (noneOf "\n")
  contents <- manyTill anyChar (try  (string "#+END_SRC\n"))
 -- _ <- string "\n"
  return $   Export (Just (pack exportLang)) (pack contents) 

parseSrcNoLang = do
  string "#+BEGIN_SRC"
  contents <- manyTill anyChar (try  (string "#+END_SRC\n"))
  return $   Export Nothing (pack contents) 

parseSrc = try parseSrcLang <|> parseSrcNoLang


parseExample = do
  string "#+BEGIN_EXAMPLE"
  contents <- manyTill anyChar (try  (string "#+END_EXAMPLE\n"))
  return $   Example (pack contents) 


parseTextMid :: Parser Element
parseTextMid = do
  text <- manyTill anyChar ((try ( lookAhead end) ))  :: Parser String
  return $ P (pack text)

parseTextEnd = do
  text <- many1 anyChar
  return $ P (pack text)

parseText = try parseTextMid <|> parseTextEnd

-- | used in text parser texts until we've reached eof or start of other token
end :: Parser String
end = try (string "#+BEGIN_EXPORT")
  <|>  (string "#+BEGIN_SRC")

parseElem :: Parser Element
parseElem = try parseExport
  <|> try parseExample
  <|> try parseSrc
  <|> parseText
  
fileParser :: Parser Page
fileParser =  Page <$>  many1 parseElem

main :: IO ()
main = parseFile "test.org" >>= print
