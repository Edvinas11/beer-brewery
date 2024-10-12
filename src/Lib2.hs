{-# LANGUAGE InstanceSigs #-}
module Lib2
    ( Query(..),
    parseQuery,
    State(..),
    emptyState,
    stateTransition,
    Ingredient(..),
    Beer(..),
    BeerType(..),
    AlcoholContent(..),
    parseBeerName,
    parseBeerType,
    parseNumber,
    parseIngredient,
    parseAlcoholContent,
    parseIngredients,
    parseWhitespaces,
    parseWord,
    parseBeer,
    parsePeriod,
    parseTime,
    parseProcess
    ) where

import qualified Data.Char as C
import qualified Data.List as L

type Parser a = String -> Either String (a, String)

-- | An entity which represets user input.
data Query = AddBeer Beer
           | RemoveBeer String
  deriving (Show, Eq)

data Ingredient = Malt | Hops | Yeast | Water
  deriving (Show, Eq)

data Ingredients = Ingredients String [Ingredient]
  deriving (Show, Eq)

data Beer = Beer 
  { beerName :: BeerName,
    beerType :: BeerType,
    alcoholContent :: AlcoholContent,
    ingredients :: [Ingredient]
  } 
  deriving (Show, Eq)

data BeerName = PaleAle | Guinness
  deriving (Show, Eq)

data BeerType = Lager | Ale | Stout
  deriving (Show, Eq)

data AlcoholContent = AlcoholContent Int Char
  deriving (Show, Eq)

data Time = Time Int String
  deriving (Show, Eq)

-- | An entity which represents your program's state.
data State = State
  {
    inventory :: [Beer]
  }
  deriving (Show, Eq)

-- | Creates an initial program's state.
emptyState :: State
emptyState = State {inventory = []}

parseWord :: Parser String
parseWord [] = Left "Empty string"
parseWord str =
  let (_, rest1) = case parseWhitespaces str of
        Right (s, r) -> (s, r)
        Left _ -> ("", str)
      word = L.takeWhile C.isLetter rest1
      rest = drop (length word) rest1
   in case word of
        [] -> Left "No word found"
        _ -> Right (word, rest)

parseChar :: Char -> Parser Char
parseChar _ [] = Left "Unexpected end of input"
parseChar c input =
  let input' = skipSpaces input
   in if null input'
        then Left "Unexpected end of input"
        else
          if head input' == c
            then Right (c, tail input')
            else Left $ "Expected '" ++ [c] ++ "', but found '" ++ [head input'] ++ "'"

skipSpaces :: String -> String
skipSpaces = dropWhile (== ' ')

parseWhitespaces :: Parser String
parseWhitespaces [] = Right ("", [])
parseWhitespaces s@(h : t) = if C.isSpace h then Right (" ", t) else Right ("", s)

-- <name> ::= "PaleAle" | "Guinness"
parseBeerName :: Parser BeerName
parseBeerName = \input ->
  case parseWord input of
    Right (word, rest) -> case word of
      "PaleAle" -> Right (PaleAle, rest)
      "Guinness" -> Right (Guinness, rest)
      _ -> Left "Beer name not found"
    Left e -> Left e

-- <type> ::= "Lager" | "Ale" | "Stout"
parseBeerType :: Parser BeerType
parseBeerType = \input ->
  case parseWord input of
    Right(word, rest) -> case word of
      "Lager" -> Right (Lager, rest)
      "Ale" -> Right (Ale, rest)
      "Stout" -> Right (Stout, rest)
      _ -> Left "Beer type not found"
    Left e -> Left e

-- <ingredient> ::= "(" "Malt" | "Hops" | "Yeast" | "Water" ")"
parseIngredient :: Parser Ingredient
parseIngredient = \input ->
  case parseWord input of
    Right(word, rest) -> case word of
      "Malt" -> Right (Malt, rest)
      "Hops" -> Right (Hops, rest)
      "Yeast" -> Right (Yeast, rest)
      "Water" -> Right (Water, rest)
      _ -> Left "Ingredient not found"
    Left e -> Left e

-- <ingredients> ::= <ingredient> {<ingredient>}
parseIngredients :: Parser [Ingredient]
parseIngredients input =
  case parseIngredient input of
    Right (ingredient, rest) -> 
      case parseWhitespaces rest of  -- Handle possible whitespace between ingredients
        Right (_, rest1) -> 
          case parseIngredients rest1 of  -- Recursively parse the next ingredient
            Right (ingredients, rest2) -> Right (ingredient : ingredients, rest2)
            Left _ -> Right ([ingredient], rest1)  -- No more ingredients
        Left _ -> Right ([ingredient], rest)  -- No whitespace, end of list
    Left e -> Left "Error parsing ingredients list"

-- <number> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
parseNumber :: Parser Int
parseNumber input =
  case parseWhitespaces input of
    Right (_, rest) -> 
      let digits = L.takeWhile C.isDigit rest
          restDigits = L.dropWhile C.isDigit rest
      in if not (null digits)
         then Right (read digits, restDigits)
         else Left "Invalid character: expected a digit"
    Left err -> Left err

-- combine two parsers
and2' :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
and2' c a b = \input ->
  case a input of
    Right (v1, r1) ->
      case b r1 of
        Right (v2, r2) -> Right (c v1 v2, r2)
        Left e2 -> Left e2
    Left e1 -> Left e1

-- accumulate items into a list
and2'' :: Parser a -> Parser [a] -> Parser [a]
and2'' a b = \input ->
  case a input of
    Right (v1, r1) ->
      case b r1 of
        Right (v2, r2) -> Right (v1 : v2, r2)
        Left e2 -> Left e2
    Left e1 -> Left e1

-- provide alternative parsing options
or2' :: [Parser a] -> Parser [a] -> Parser [a]
or2' [] b = \input -> b input
or2' (a : _) b = \input ->
  case b input of
    Right (v1, r1) -> Right (v1, r1)
    Left e1 ->
      case a input of
        Right (v2, r2) -> Right ([v2], r2)
        Left e2 -> Left (e1 ++ ", " ++ e2)

and4' :: (a -> b -> c -> d -> e) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
and4' f p1 p2 p3 p4 = \input -> 
  case p1 input of
    Right (v1, r1) -> 
      case p2 r1 of
        Right (v2, r2) -> 
          case p3 r2 of
            Right (v3, r3) ->
              case p4 r3 of
                Right (v4, r4) -> Right (f v1 v2 v3 v4, r4)
                Left e4 -> Left e4
            Left e3 -> Left e3
        Left e2 -> Left e2
    Left e1 -> Left e1

and3' :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
and3' d a b c = \input ->
  case a input of
    Right (v1, r1) ->
      case b r1 of
        Right (v2, r2) ->
          case c r2 of
            Right (v3, r3) -> Right (d v1 v2 v3, r3)
            Left e3 -> Left e3
        Left e2 -> Left e2
    Left e1 -> Left e1

and6' :: (a -> b -> c -> d -> e -> f -> g) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser g
and6' comb p1 p2 p3 p4 p5 p6 = \input ->
  case p1 input of
    Right (v1, r1) ->
      case p2 r1 of
        Right (v2, r2) ->
          case p3 r2 of
            Right (v3, r3) ->
              case p4 r3 of
                Right (v4, r4) ->
                  case p5 r4 of
                    Right (v5, r5) ->
                      case p6 r5 of
                        Right (v6, r6) -> Right (comb v1 v2 v3 v4 v5 v6, r6)
                        Left e6 -> Left e6
                    Left e5 -> Left e5
                Left e4 -> Left e4
            Left e3 -> Left e3
        Left e2 -> Left e2
    Left e1 -> Left e1

or6' :: Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a -> Parser a
or6' p1 p2 p3 p4 p5 p6 = \input ->
  case p1 input of
    Right r1 -> Right r1
    Left e1 -> case p2 input of
      Right r2 -> Right r2
      Left e2 -> case p3 input of
        Right r3 -> Right r3
        Left e3 -> case p4 input of
          Right r4 -> Right r4
          Left e4 -> case p5 input of
            Right r5 -> Right r5
            Left e5 -> case p6 input of
              Right r6 -> Right r6
              Left e6 -> Left (e1 ++ "; " ++ e2 ++ "; " ++ e3 ++ "; " ++ e4 ++ "; " ++ e5 ++ "; " ++ e6)

and10' :: (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k) 
       -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f 
       -> Parser g -> Parser h -> Parser i -> Parser j -> Parser k
and10' comb p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 = \input -> 
  case p1 input of
    Right (v1, r1) -> 
      case p2 r1 of
        Right (v2, r2) -> 
          case p3 r2 of
            Right (v3, r3) -> 
              case p4 r3 of
                Right (v4, r4) -> 
                  case p5 r4 of
                    Right (v5, r5) -> 
                      case p6 r5 of
                        Right (v6, r6) -> 
                          case p7 r6 of
                            Right (v7, r7) -> 
                              case p8 r7 of
                                Right (v8, r8) -> 
                                  case p9 r8 of
                                    Right (v9, r9) -> 
                                      case p10 r9 of
                                        Right (v10, r10) -> 
                                          Right (comb v1 v2 v3 v4 v5 v6 v7 v8 v9 v10, r10)
                                        Left e10 -> Left e10
                                    Left e9 -> Left e9
                                Left e8 -> Left e8
                            Left e7 -> Left e7
                        Left e6 -> Left e6
                    Left e5 -> Left e5
                Left e4 -> Left e4
            Left e3 -> Left e3
        Left e2 -> Left e2
    Left e1 -> Left e1

-- <alcohol_content> ::= <number> "%"
parseAlcoholContent :: Parser AlcoholContent
parseAlcoholContent = 
  and2' (\num _ -> AlcoholContent num '%') parseNumber (parseChar '%')

-- <beer> ::= <name> <type> <alcohol_content> <ingredients>
parseBeer :: Parser Beer
parseBeer =
  and4'
    (\nameStr typeStr contentStr ingredientsStr -> Beer nameStr typeStr contentStr ingredientsStr)
    parseBeerName
    parseBeerType
    parseAlcoholContent
    (and2' (\_ ingredients -> ingredients) 
      (parseChar '(') 
      (and2' (\ingredients _ -> ingredients) 
        parseIngredients (parseChar ')')))

parseMinutes :: Parser String
parseMinutes = \input -> case parseWord input of
  Right ("minutes", rest) -> Right ("minutes", rest)
  _ -> Left "Expected minutes"

parseHour :: Parser String
parseHour = \input -> case parseWord input of
  Right ("hour", rest) -> Right ("hour", rest)
  _ -> Left "Expected 'hour'"

parseWeeks :: Parser String
parseWeeks = \input -> case parseWord input of
  Right ("weeks", rest) -> Right ("weeks", rest)
  _ -> Left "Expected 'weeks'"

parseMonth :: Parser String
parseMonth = \input -> case parseWord input of
  Right ("month", rest) -> Right ("month", rest)
  _ -> Left "Expected 'month'"

parseMonths :: Parser String
parseMonths = \input -> case parseWord input of
  Right ("months", rest) -> Right ("months", rest)
  _ -> Left "Expected 'months'"

-- <period> ::= "minutes" | "hour" | "weeks" | "month" | "months"
parsePeriod :: Parser String
parsePeriod = or6' parseMinutes parseHour parseWeeks parseMonth parseMonths parseWord

-- <time> ::= <number> <period>
parseTime :: Parser Time
parseTime = and2' Time parseNumber parsePeriod

parseMash :: Parser String
parseMash = \input -> case parseWord input of
  Right ("Mash", rest) -> Right ("Mash", rest)
  _ -> Left "Expected Mash"

parseBoil :: Parser String
parseBoil = \input -> case parseWord input of
  Right ("Boil", rest) -> Right ("Boil", rest)
  _ -> Left "Expected Mash"

parseFerment :: Parser String
parseFerment = \input -> case parseWord input of
  Right ("Ferment", rest) -> Right ("Ferment", rest)
  _ -> Left "Expected Mash"

parseCondition :: Parser String
parseCondition = \input -> case parseWord input of
  Right ("Condition", rest) -> Right ("Condition", rest)
  _ -> Left "Expected Mash"

-- <process> ::= "(" "Mash" <time> "Boil" <time> "Ferment" <time> "Condition" <time> ")"
parseProcess :: Parser (String, Time, String, Time, String, Time, String, Time)
parseProcess =
  and10'
    (\_ mash time1 boil time2 ferment time3 condition time4 _ ->
      (mash, time1, boil, time2, ferment, time3, condition, time4))
    (parseChar '(')      -- Opening parenthesis
    parseMash            -- "Mash"
    parseTime            -- <time> for Mash
    parseBoil            -- "Boil"
    parseTime            -- <time> for Boil
    parseFerment         -- "Ferment"
    parseTime            -- <time> for Ferment
    parseCondition       -- "Condition"
    parseTime            -- <time> for Condition
    (parseChar ')')      -- Closing parenthesis

-- | Parses user's input.
-- The function must have tests.
parseQuery :: String -> Either String Query
parseQuery _ = Left "Not implemented 2"

-- | Updates a state according to a query.
-- This allows your program to share the state
-- between repl iterations.
-- Right contains an optional message to print and
-- an updated program's state.
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition _ _ = Left "Not implemented 3"