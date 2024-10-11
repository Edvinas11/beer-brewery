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
    parseBeer
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
parseChar c [] = Left ("Cannot find " ++ [c] ++ " in an empty input")
parseChar c s@(h : t) = if c == h then Right (c, t) else Left (c : " is not found in " ++ s)

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

-- <alcohol_content> ::= <number> "%"
parseAlcoholContent :: Parser AlcoholContent
parseAlcoholContent = 
  and2' (\num _ -> AlcoholContent num '%') parseNumber (parseChar '%')

-- <beer> ::= <name> <type> <alcohol_content> <ingredients>
parseBeer :: Parser Beer
parseBeer = and4' Beer parseBeerName parseBeerType parseAlcoholContent parseIngredients

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