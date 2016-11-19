import Data.List
import Data.Char

-- 1. Createa a data structure that captures the phone layout above.
data DaPhone   = DaPhone [LetterKey] EmptyKey SpaceKey CapKey deriving (Show, Eq)
data LetterKey = LetterK Char String deriving (Show, Eq)
data EmptyKey  = EmptyK Char deriving (Show, Eq)
data SpaceKey  = SpaceK Char deriving (Show, Eq)
data CapKey    = CapK Char deriving (Show, Eq)
data NoKey     = NoKey deriving (Show, Eq)

thisPhone = DaPhone
  [ LetterK '2' "abc"
  , LetterK '3' "def"
  , LetterK '4' "ghi"
  , LetterK '5' "jkl"
  , LetterK '6' "mno"
  , LetterK '7' "pqrs"
  , LetterK '8' "tuv"
  , LetterK '9' "wxyz"
  , LetterK '#' ".,"
  ]
  (EmptyK '1')
  (SpaceK '0')
  (CapK '*')

-- 2. convert the following conversations into the keypresses required to
-- express them.
convo :: [String]
convo =
  ["Wanna play 20 questions"
  , "Ya"
  , "U 1st haha"
  , "Lol ok. Have y ever tasted alcohol lol"
  , "Lol ya"
  , "Wow ur cool haha. Ur turn"
  , "Ok. Do u think I am pretty Lol"
  , "Lol ya"
  , "Haha tansk just making sure rofl ur turn"
  ]

type Digit = Char
type Presses = Int

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps phone@(DaPhone keys (EmptyK empty) (SpaceK space) (CapK cap)) char
  | isUpper char = (cap, 1):(reverseTaps phone (toLower char))
  | char == ' '  = [(space, 1)]
  | otherwise    = getPresses keys char
  where getPresses [] _ = [('!', 0)]
        getPresses ((LetterK digit letters):rest) char =
          case elemIndex char letters of
            Nothing    -> getPresses rest char
            Just index -> [(digit, index + 1)]

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead phone = concat . (map (reverseTaps phone))

-- map (cellPhonesDead thisPhone) convo

-- 3. How many times do digits need to be pressed for each message?

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = (foldl (+) 0) . (map snd)

-- 4. What is the most popular letter for each message? What was its
-- cost?
--
-- I don't understand what this is asking

-- 5. What was the most popular letter overall? What was the most
-- popular word?
--
-- I don't understand what this is asking
