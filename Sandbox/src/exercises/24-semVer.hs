import Control.Applicative

import Text.Trifecta
import Test.Hspec


data NumberOrString =
    NOSS String
  | NOSI Integer
  deriving (Eq, Show)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer = SemVer Major Minor Patch Release Metadata deriving (Eq, Show)

parseSemVer :: Parser SemVer
parseSemVer = do
  (major, minor, patch) <- parseCore
  release <- parseRelease
  metadata <- parseMetadata
  return $ SemVer major minor patch release metadata

parseCore :: Parser (Major, Minor, Patch)
parseCore = do
  major <- integer
  _ <- char '.'
  minor <- integer
  _ <- char '.'
  patch <- integer
  return $ (major, minor, patch)

parseRelease :: Parser [NumberOrString]
parseRelease = try (char '-' >> some parseNOS) <|> return []

parseMetadata :: Parser [NumberOrString]
parseMetadata = try (char '+' >> some parseNOS) <|> return []

parseNOS :: Parser NumberOrString
parseNOS = skipMany (char '.') >> (try (NOSI <$> integer) <|> try (NOSS <$> some letter))


maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

main :: IO ()
main = hspec $ do
  describe "parseSemVer" $ do

    it "parses simple major.minor.patch" $ do
      let expected = SemVer 2 1 1 [] []
          result = parseString parseSemVer mempty "2.1.1"
      maybeSuccess result `shouldBe` Just expected

    it "parses pre-release data" $ do
      let expected = SemVer 1 0 0 [NOSS "x", NOSI 7, NOSS "z", NOSI 92] []
          result = parseString parseSemVer mempty "1.0.0-x.7.z.92"
      maybeSuccess result `shouldBe` Just expected

    it "parses release and metadata" $ do
      let expected = SemVer 1 0 0 [NOSS "alpha", NOSI 701] [NOSS "sha", NOSI 543]
          result = parseString parseSemVer mempty "1.0.0-alpha.701+sha.543"
      maybeSuccess result `shouldBe` Just expected

  describe "parseRelease" $ do
    it "parses a release" $ do
      let expected = [NOSS "alpha", NOSI 123, NOSS "z", NOSS "q"]
          result = parseString parseRelease mempty "-alpha.123.z.q"
      maybeSuccess result `shouldBe` Just expected

    it "parses a non-release" $ do
      let expected = []
          result = parseString parseRelease mempty ""
      maybeSuccess result `shouldBe` Just expected

  describe "parseMetadata" $ do
    it "parses metadata" $ do
      let expected = [NOSS "sha", NOSI 12345]
          result = parseString parseMetadata mempty "+sha.12345"
      maybeSuccess result `shouldBe` Just expected

    it "parses non-metadata" $ do
      let expected = []
          result = parseString parseMetadata mempty ""
      maybeSuccess result `shouldBe` Just expected
