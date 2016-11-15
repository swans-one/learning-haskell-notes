module Authors where

data Fiction = Fiction deriving Show
data NonFiction = NonFiction deriving Show

data BookType = FictionBook Fiction
              | NonFictionBook NonFiction
              deriving Show

type AuthorName = String

-- non-normal form:
-- data Author = Author (AuthorName, BookType)

-- normal form:
data Author =
    Fiction AuthorName
  | NonFiction AuthorName
  deriving (Eq, Show)
