{-# LANGUAGE OverloadedStrings #-}

-- Note: OverloadedStrings makes string literals polymorphic over the
-- IsString type class.

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta

badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

parseDecimal :: Parser Float
parseDecimal = do
  first <- decimal
  char '.'
  last <- decimal
  return (fromIntegral first + toDecimal last)

toDecimal :: Integral a => a -> Float
toDecimal 0 = 0
toDecimal x = x' / mantisa x'
  where
    x' = fromIntegral x
    mantisa = (10 ^) . (+ 1) . floor . logBase 10

type DecFrac = Either Rational Float

parseFracDec :: Parser DecFrac
parseFracDec = (Left <$> try parseFraction) <|> (Right <$> try parseDecimal)

main :: IO ()
main = do
  print $ parseString parseFraction mempty shouldWork
  print $ parseString parseFraction mempty shouldAlsoWork
  print $ parseString parseFraction mempty alsoBad
  print $ parseString parseFraction mempty badFraction
