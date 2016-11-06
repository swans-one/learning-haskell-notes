import Data.Time
data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDateUTCTime
                  deriving (Eq, Ord, Show)


theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  ]


filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr []
