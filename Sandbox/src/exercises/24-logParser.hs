import Control.Applicative
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import Text.Trifecta
import Test.Hspec


data Log = Log [Comment] [Day] deriving (Eq, Show)
data Day = Day
  { getDate::Date
  , getDayComment::Comment
  , getEntries::[Entry]
  }
  deriving (Eq, Show)
data Entry = Entry
  { getTime::Time
  , getActivity::Activity
  , getEntryComment::Comment
  }
  deriving (Eq, Show)

type Comment = Maybe String
type Time = Int
type Activity = String
type Date = (Int, Int, Int)

type Aggregate = M.Map Activity Time

aggregateLog :: Log -> Aggregate
aggregateLog (Log comments days) =
  foldl combine M.empty days
  where unionTimes = M.unionWith (+)
        combine = flip (unionTimes . aggregateEntries . getEntries)

aggregateEntries :: [Entry] -> Aggregate
aggregateEntries = snd . (foldl (flip updateAggregates) (Nothing, M.empty))

updateAggregates :: Entry -> (Maybe Entry, Aggregate) -> (Maybe Entry, Aggregate)
updateAggregates next (Nothing, aggs) = (Just next, aggs)
updateAggregates next ((Just last), aggs) =
  case M.member lastActivity aggs of
    True -> (Just next, M.adjust (+ timeDiff) lastActivity aggs)
    False -> (Just next, M.insert lastActivity timeDiff aggs)
  where lastActivity = (getActivity last)
        timeDiff = (getTime next) - (getTime last)


parseLog :: Parser Log
parseLog = do
  blankLines
  comments <- parseComments
  blankLines
  days <- many (parseDay <* blankLines)
  return $ Log comments days

parseDay :: Parser Day
parseDay = liftA3 Day parseDate parseComment (some parseEntry)

parseEntry :: Parser Entry
parseEntry = do
  time <- parseTime
  activity <- parseActivity
  comment <- parseComment
  many (char '\n')
  return $ Entry time activity comment

parseDate :: Parser Date
parseDate = do
  string "# "
  year <- count 4 digit
  char '-'
  month <- count 2 digit
  char '-'
  day <- count 2 digit
  many $ (char '\n' <|> char ' ')
  return (read year, read month, read day)

parseTime :: Parser Time
parseTime = do
  hours <- count 2 digit
  char ':'
  minutes <- count 2 digit
  return $ read hours * 60 + read minutes

parseActivity :: Parser Activity
parseActivity = char ' ' >> strip <$> sentences

parseComments :: Parser [Comment]
parseComments = many (Just <$> parseCommentString)

parseComment :: Parser Comment
parseComment = try (fmap Just parseCommentString) <|> return Nothing

parseCommentString :: Parser String
parseCommentString = do
  string "--"
  many (char ' ')
  com <- sentences
  some (char '\n')
  return com

blankLines :: Parser String
blankLines = many $ oneOf ['\n', ' ']

sentences :: Parser String
sentences = some (letter <|> char ' ' <|> punctuation)

punctuation :: Parser Char
punctuation = oneOf ".,!?;:@#$%^&*()_=+'/<>~`\""

strip :: String -> String
strip = T.unpack . T.strip . T.pack

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

exampleDay :: String
exampleDay = L.intercalate
    "\n"
    ["# 2025-02-05 -- This is a comment"
    , "08:00 Breakfast"
    , "09:00 Sanitizing moisture collector"
    , "11:20 Exercise   -- Worked hard"
    , "12:15 Lunch and reading"
    ]

exampleLog :: String
exampleLog = L.intercalate
    "\n"
    [ ""
    , "-- My personal log"
    , "-- By Erik Swanson"
    , ""
    ,  "# 2025-02-05 -- This is a comment"
    , "08:00 Breakfast"
    , "09:00 Sanitizing moisture collector"
    , "11:20 Exercise   -- Worked hard"
    , "12:15 Lunch"
    , "13:00 Reading"
    , "22:00 Sleep -- good night"
    , ""
    , "# 2025-02-06"
    , ""
    , "08:00 Breakfast -- cereal"
    , "09:45 Work"
    , "17:00 Breath of the wild"
    , "22:00 Sleep"
    ]


htest :: IO ()
htest = hspec $ do
  describe "updateAggregates" $ do
    it "inserts properly" $ do
      let last = (Entry 450 "last" Nothing)
          next = (Entry 500 "next" Nothing)
          expected = (Just next, M.fromList [(getActivity last, 50)])
          result = updateAggregates next (Just last, M.empty)
      result `shouldBe` expected

    it "updates properly" $ do
      let last = (Entry 450 "last" Nothing)
          next = (Entry 500 "next" Nothing)
          expected = (Just next, M.fromList [(getActivity last, 100)])
          result = updateAggregates next (Just last, M.fromList [(getActivity last, 50)])
      result `shouldBe` expected

    it "handles no last" $ do
      let last = Nothing
          next = (Entry 500 "next" Nothing)
          expected = (Just next, M.empty)
          result = updateAggregates next (Nothing, M.empty)
      result `shouldBe` expected

  describe "parseLog" $ do
    it "parses nothing" $ do
      let expected = Log [] []
          result = parseString parseLog mempty ""
      maybeSuccess result `shouldBe` Just expected

  describe "parseDay" $ do
    it "parses and example day" $ do
      let expected = Day (2025, 2, 5)
                         (Just "This is a comment")
                         [ Entry 480 "Breakfast" Nothing
                         , Entry 540 "Sanitizing moisture collector" Nothing
                         , Entry 680 "Exercise" (Just "Worked hard")
                         , Entry 735 "Lunch and reading" Nothing
                         ]
          result = parseString parseDay mempty exampleDay
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

  describe "parseDate" $ do
    it "parses a valid date with comment" $ do
      let expected = (2050, 12, 8)
          result = parseString parseDate mempty "# 2050-12-08 -- comment"
      maybeSuccess result `shouldBe` Just expected

    it "parses a valid date with newline" $ do
      let expected = (2050, 12, 8)
          result = parseString parseDate mempty "# 2050-12-08 \n"
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
