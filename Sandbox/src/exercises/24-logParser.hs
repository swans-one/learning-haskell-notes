import Control.Applicative
import qualified Data.Text as T

import Text.Trifecta
import Test.Hspec

data Log = Log [Comment] [Day] deriving (Eq, Show)
data Day = Day Date Comment [Entry] deriving (Eq, Show)
data Entry = Entry Time Activity Comment deriving (Eq, Show)

type Comment = Maybe String
type Time = Int
type Activity = String
type Date = String

parseLog :: Parser Log
parseLog = do
  return $ Log [] []

parseEntry :: Parser Entry
parseEntry = do
  time <- parseTime
  activity <- parseActivity
  comment <- parseComment
  many (char '\n')
  return $ Entry time activity comment

parseTime :: Parser Time
parseTime = do
  hours <- count 2 digit
  char ':'
  minutes <- count 2 digit
  return $ read hours * 60 + read minutes

parseActivity :: Parser Activity
parseActivity = char ' ' >> strip <$> sentences

parseComment :: Parser Comment
parseComment = try (fmap Just parseCommentString) <|> return Nothing

parseCommentString :: Parser String
parseCommentString = do
  string "--"
  many (char ' ')
  com <- sentences
  some (char '\n')
  return com

sentences :: Parser String
sentences = some (letter <|> char ' ' <|> punctuation)

punctuation :: Parser Char
punctuation = oneOf ".,!?;:@#$%^&*()_=+'/<>~`\""

strip :: String -> String
strip = T.unpack . T.strip . T.pack

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

htest :: IO ()
htest = hspec $ do
  describe "parseLog" $ do
    it "parses nothing" $ do
      let expected = Log [] []
          result = parseString parseLog mempty ""
      maybeSuccess result `shouldBe` Just expected

  describe "parseEntry" $ do
    it "parses without comment" $ do
      let expected = Entry 20 "Eat food" Nothing
          result = parseString parseEntry mempty "00:20 Eat food \n"
      maybeSuccess result `shouldBe` Just expected

    it "parses with a comment" $ do
      let expected = Entry 20 "Eat food" (Just "always do this")
          result = parseString parseEntry mempty "00:20 Eat food -- always do this\n"
      maybeSuccess result `shouldBe` Just expected

  describe "parseTime" $ do
    it "parses time to minutes" $ do
      let expected = 620
          result = parseString parseTime mempty "10:20"
      maybeSuccess result `shouldBe` Just expected

  describe "parseActivity" $ do
    it "parses an activity" $ do
      let expected = "Breakfast at Tiffanies"
          result = parseString parseActivity mempty " Breakfast at Tiffanies"
      maybeSuccess result `shouldBe` Just expected

  describe "parseComment" $ do
    it "parses a comment" $ do
      let expected = Just "This is a comment"
          input = "--  This is a comment\n\n"
          result = parseString parseComment mempty input
      maybeSuccess result `shouldBe` Just expected

    it "gives nothing for empty string" $ do
      let expected = Nothing
          input = ""
          result = parseString parseComment mempty input
      maybeSuccess result `shouldBe` Just expected

  describe "parseCommentString" $ do
    it "parses a comment" $ do
      let expected = "This is a comment"
          input = "--  This is a comment\n\n"
          result = parseString parseCommentString mempty input
      maybeSuccess result `shouldBe` Just expected
