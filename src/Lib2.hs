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
    parseIngredient,
    parseBeerType,
    parseNumber,
    parseAlcoholContent,
    parseBeer
    ) where

-- | An entity which represets user input.
data Query = AddBeer Beer
           | RemoveBeer String
  deriving (Show, Eq)

data Ingredient = Malt | Hops | Yeast | Water
  deriving (Show, Eq)

<<<<<<< HEAD
data Beer = Beer 
  { beerName :: BeerName,
    beerType :: BeerType,
    alcoholContent :: AlcoholContent
    -- ingredients :: [Ingredient]
  } 
  deriving (Show, Eq)
=======
-- Data Types
>>>>>>> d324d6d8f2c645e408b1ffa566d7faf250c3b683

data BeerName = PaleAle | Guinness
  deriving (Show, Eq)

data BeerType = Lager | Ale | Stout
  deriving (Show, Eq)

data AlcoholContent = AlcoholContent Int String
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

-- <name> ::= "Pale Ale" | "Guinness"
parseBeerName :: String -> Either String BeerName
parseBeerName "PaleAle" = Right PaleAle
parseBeerName "Guinness" = Right Guinness
parseBeerName _ = Left "Invalid beer name"

-- <ingredient> ::= "(" "Malt" | "Hops" | "Yeast" | "Water" ")"
parseIngredient :: String -> Either String Ingredient
parseIngredient "Malt" = Right Malt
parseIngredient "Hops" = Right Hops
parseIngredient "Yeast" = Right Yeast
parseIngredient "Water" = Right Water
parseIngredient _ = Left "Invalid ingredient"

-- <type> ::= "Lager" | "Ale" | "Stout"
parseBeerType :: String -> Either String BeerType
parseBeerType "Lager" = Right Lager
parseBeerType "Ale" = Right Ale
parseBeerType "Stout" = Right Stout
parseBeerType _ = Left "Invalid beer type"

-- <number> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
parseNumber :: String -> (Int, String)
parseNumber [] = error "No input to parse"
parseNumber (x:xs)
  | x >= '0' && x <= '9' = (fromEnum x - fromEnum '0', xs)
  | otherwise = error "Invalid character: expected a digit"

-- <alcohol_content> ::= <number> "%"
parseAlcoholContent :: String -> Either String AlcoholContent
parseAlcoholContent input =
  let (number, rest) = parseNumber input
  in case rest of
    ('%':[]) -> Right $ AlcoholContent number "%"
    _        -> Left "Invalid format: percentage sign is missing"

-- <beer> ::= "(" <name> <type> <alcohol_content> <ingredients> ")"
parseBeer :: String -> Either String Beer
parseBeer input = case words input of
  (beerNameStr : beerTypeStr : alcoholContentStr : _) -> case parseBeerName beerNameStr of
    Right beerName -> case parseBeerType beerTypeStr of
      Right beerType -> case parseAlcoholContent alcoholContentStr of
        Right alcoholContent -> Right $ Beer beerName beerType alcoholContent
        Left e -> Left e
      Left e -> Left e
    Left e -> Left e
  _ -> Left "Invalid beer format"

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