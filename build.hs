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

    
jsonURL :: String
jsonURL = "http://ec2-3-95-164-121.compute-1.amazonaws.com:3002/tasks"

getJSON :: IO B.ByteString
getJSON = simpleHttp jsonURL


getTasksFromVillefort :: IO [Task]
getTasksFromVillefort = do
  d <- (eitherDecode <$> getJSON) :: IO (Either String [Task])
  case d of
    Left err -> pure [Task 0 ("Error: " ++ show err) ""  "" "" 0 0 ] 
    Right ps ->  return ps

  
-- | our task datatype
data Task = Task {rid :: Int,
                  title :: String,
                  description :: String,
                  due :: String,
                  subject :: String,
                  time :: Int,
                  dueIn :: Integer
                 }
          deriving (Generic)


instance FromJSON Task
instance ToJSON Task
instance Eq Task where
  task == task1 = rid task == rid task1

instance Show Task where show = showTask

showTask :: Task -> String
showTask (Task uid title desc due _ _ dueIn) = do
  "* " ++ title ++ " " ++  show dueIn ++ "\n"  ++ "UID:" ++ (show uid) ++ "\n" ++ desc 

parseTask :: Parser Task
parseTask = do
  -- | snap onto each org header
  _ <- string "* " 
  rawTitle <- many1 (noneOf "\n")
  let splits = splitOn " " (pack rawTitle)
  let title = intercalate " " $ init splits
  let days  = last splits
  _ <- many1 (noneOf "U")
  _ <- string "UID:"
  uid <- many1 digit
  longDes <- manyTill anyChar (try  (string "\n* "))
  let des = Prelude.drop 1  longDes
  return $ Task (read uid :: Int) (unpack title)  des  "" "" 0 (read (unpack days) :: Integer)

more :: Parser Task
more = do
  -- | snap onto each org header 
  rawTitle <- many1 (noneOf "\n")
  let splits = splitOn " " (pack rawTitle)
  let title = intercalate " " $ init splits
  let days  = last splits
  _ <- many1 (noneOf "U")
  _ <- string "UID:"
  uid <- many1 digit
  longDes <- manyTill anyChar (try  (string "\n* ") <|> (eof >> pure ""))
  let des = Prelude.drop 1  longDes
  return $ Task (read uid :: Int) (unpack title)  des  "" "" 0 (read (unpack days) :: Integer)

  




-- | Page Data type
data Page = Page [Element]

instance Show Page where show = showPage

showPage :: Page -> String
showPage (Page elems) = Prelude.concat $  Prelude.map (\x -> show x ++ "\n") elems

-- | Element datatype
data Element = P Text
  | Export {lang :: Maybe Text, contents :: Text}
  | Src {lang :: Maybe Text, contents :: Text}
  | Dumb Text
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

parseSrc = do
  string "#+BEGIN_SRC "
  srcLang <- many1 (noneOf "\n")
  contents <- manyTill anyChar (try  (string "#+END_SRC"))
  return $ Export (Just (pack srcLang)) (pack contents)

parseTextMid :: Parser Element
parseTextMid = do
  text <- manyTill anyChar ((try ( lookAhead end) ))  :: Parser String
  return $ P (pack text)

parseTextEnd = do
  text <- many1 anyChar
  return $ P (pack text)

parseText = try parseTextMid <|> parseTextEnd

-- | parse texts until we've reached eof or start of other token
end :: Parser String
end = (string "#+BEGIN_EXPORT")
  <|>  (string "#+BEGIN_SRC")

parseElem :: Parser Element
parseElem = parseExport
  <|> try parseText
  
fileParser :: Parser Page
fileParser =  Page <$>  many1 parseElem

main :: IO ()
main = parseFile "test.org" >>= print
