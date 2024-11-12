{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Parsers
 (
    Query (..),
    Ingredient (..),
    Ingredients (..),
    Beer (..),
    Process (..),
    Recipe (..),
    BeerName (..),
    BeerType (..),
    AlcoholContent (..),
    Time (..),
    Bag (..),
    Bags (..),
    Parser (..),
    parseBeerName,
    parseBeerType,
    parseIngredient,
    parseAlcoholContent,
    parsePeriod,
    parseTime,
    parseProcess,
    parseIngredients,
    parseIngredientList,
    parseBeer,
    parseRecipe,
    parseBags,
    parseBag,
    parseChar,
    char,
    parseInt,
    parseLiteral,
    parseCommand,
    parseAddIngredients,
    parseAddBags,
    parseView,
    parseBrewBeer
 )
where

import Control.Applicative (Alternative (empty), optional, (<|>))
import Data.Char (isDigit)

-- | An entity which represets user input.
data Query = AddIngredients [Ingredient]
           | AddBags Bags
           | BrewBeer Beer
           | View
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

data Process = Process
  { 
    mashTime :: Time, 
    boilTime :: Time, 
    fermentTime :: Time, 
    conditionTime :: Time
  } deriving (Show, Eq)

data Recipe = Recipe
  {
    recipeType :: BeerType,
    recipeIngredients :: [Ingredient],
    recipeProcess :: Process
  } deriving (Show, Eq)

data BeerName = PaleAle | Guinness
  deriving (Show, Eq)

data BeerType = Lager | Ale | Stout
  deriving (Show, Eq)

data AlcoholContent = AlcoholContent Int Char
  deriving (Show, Eq)

data Time = Time Int String
  deriving (Show, Eq)

data Bag = BagWithIngredients [Ingredient] | BagWithBagsAndIngredients [Bag] [Ingredient]
  deriving (Show, Eq)

type Bags = [Bag]

newtype Parser a = P {parse :: String -> Either String (a, String)}

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = do
    a <- p
    return $ f a

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = P $ \str -> Right (x, str)
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  pf <*> pa = do
    f <- pf
    f <$> pa

instance Alternative Parser where
  empty :: Parser a
  empty = P $ \_ -> Left "Failed to parse"
  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) p1 p2 = P $ \str -> case parse p1 str of
    Right (v, r) -> Right (v, r)
    Left _ -> parse p2 str

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (>>=) pa f = P $ \str -> case parse pa str of
    Left e -> Left e
    Right (a, r) -> parse (f a) r

parseCommand :: Parser Query
parseCommand = parseAddIngredients <|> parseAddBags <|> parseView <|> parseBrewBeer

-- <name> ::= "PaleAle" | "Guinness"
parseBeerName :: Parser BeerName
parseBeerName =
    (parseLiteral "PaleAle" >> return PaleAle) <|>
    (parseLiteral "Guinness" >> return Guinness)

-- <type> ::= "Lager" | "Ale" | "Stout"
parseBeerType :: Parser BeerType
parseBeerType =
    (parseLiteral "Lager" >> return Lager) <|>
    (parseLiteral "Ale" >> return Ale) <|>
    (parseLiteral "Stout" >> return Stout)

-- <ingredient> ::= "Malt" | "Hops" | "Yeast" | "Water"
parseIngredient :: Parser Ingredient
parseIngredient = 
    (parseLiteral "Malt" >> return Malt) <|>
    (parseLiteral "Hops" >> return Hops) <|>
    (parseLiteral "Yeast" >> return Yeast) <|>
    (parseLiteral "Water" >> return Water)

-- <alcohol_content> ::= <number> "%"
parseAlcoholContent :: Parser AlcoholContent
parseAlcoholContent = do
  number <- parseInt
  percentSign <- parseChar '%'
  return (AlcoholContent number percentSign)

-- <period> ::= "minutes" | "hour" | "weeks" | "month" | "months"
parsePeriod :: Parser String
parsePeriod = 
    (parseLiteral "minutes" >> return "minutes") <|>
    (parseLiteral "hour" >> return "hour") <|>
    (parseLiteral "weeks" >> return "weeks") <|>
    (parseLiteral "month" >> return "month") <|>
    (parseLiteral "months" >> return "months")

-- <time> ::= <number> <period>
parseTime :: Parser Time
parseTime = do
    tNumber <- parseInt
    tPeriod <- parsePeriod
    return $ Time tNumber tPeriod

-- <process> ::= "(" "Mash" <time> "Boil" <time> "Ferment" <time> "Condition" <time> ")"
parseProcess :: Parser Process
parseProcess = do
  _ <- parseChar '('
  _ <- parseLiteral "Mash"
  mash <- parseTime
  _ <- skipSpaces'
  _ <- parseLiteral "Boil"
  boil <- parseTime
  _ <- skipSpaces'
  _ <- parseLiteral "Ferment"
  ferment <- parseTime
  _ <- skipSpaces'
  _ <- parseLiteral "Condition"
  condition <- parseTime
  _ <- parseChar ')'
  return (Process mash boil ferment condition)

-- <ingredients> ::= "(" <ingredient> | <ingredient> <ingredients> ")"
parseIngredients :: Parser [Ingredient]
parseIngredients = do
    _ <- parseChar '('
    ingredients <- parseIngredientList
    _ <- parseChar ')'
    return ingredients

-- Helper function for parsing recursive <ingredients>
parseIngredientList :: Parser [Ingredient]
parseIngredientList = do
    first <- parseIngredient
    rest <- (parseIngredientList <|> pure [])
    return (first : rest)

-- <beer> ::= "(" <name> <type> <alcohol_content> <ingredients> ")"
parseBeer :: Parser Beer
parseBeer = do
  _ <- parseChar '('
  name <- parseBeerName
  _ <- skipSpaces'
  bType <- parseBeerType
  _ <- skipSpaces'
  alcContent <- parseAlcoholContent
  _ <- skipSpaces'
  ingr <- parseIngredients
  _ <- parseChar ')'
  return (Beer name bType alcContent ingr)

-- <recipe> ::= "(" <type> <ingredients> <process> ")"
parseRecipe :: Parser Recipe
parseRecipe = do
  _ <- parseChar '('
  bType <- parseBeerType
  _ <- skipSpaces'
  ingredients <- parseIngredients
  _ <- skipSpaces'
  process <- parseProcess
  _ <- parseChar ')'
  return (Recipe bType ingredients process)

-- <bags> ::= "(" <bag> ")" | "(" <bag> <bags> ")"
parseBags :: Parser [Bag]
parseBags = do
  _ <- parseChar '('
  firstBag <- parseBag
  restBags <- (do
                _ <- skipSpaces'
                bags <- parseBags
                return bags) <|> pure []
  _ <- parseChar ')'
  return (firstBag : restBags)

-- <bag> ::= <ingredients> | "(" <bags> <ingredients> ")"
parseBag :: Parser Bag
parseBag = 
    (do
      ingredients <- parseIngredients
      return (BagWithIngredients ingredients)) <|>
    (do
      _ <- parseChar '('
      nestedBags <- parseBags
      _ <- skipSpaces'
      ingredients <- parseIngredients
      _ <- parseChar ')'
      return (BagWithBagsAndIngredients nestedBags ingredients))

parseChar :: Char -> Parser Char
parseChar = char

char :: Char -> Parser Char
char c = sat (== c)

-- Utility Parsers
sat :: (Char -> Bool) -> Parser Char
sat p = P $ \case
  [] -> Left "Empty String"
  s@(x : xs) -> if p x then Right (x, xs) else Left $ "Could not recognize: " ++ s

skipSpaces :: String -> String
skipSpaces = dropWhile (== ' ')

skipSpaces' :: Parser ()
skipSpaces' = P $ \input ->
  let input' = dropWhile (== ' ') input
   in Right ((), input')

parseLiteral :: String -> Parser String
parseLiteral [] = return []
parseLiteral (x : xs) = do
  _ <- skipSpaces'
  _ <- parseChar x
  parseLiteral xs

parseInt :: Parser Int
parseInt = P $ \input ->
  let (digits, rest) = span isDigit (skipSpaces input)
   in if null digits
        then Left "Expected an integer"
        else Right (read digits, rest)

-- Individual command parsers
parseAddIngredients :: Parser Query
parseAddIngredients = do
  _ <- parseLiteral "AddIngredients"
  ingredients <- parseIngredients
  return $ AddIngredients ingredients

parseAddBags :: Parser Query
parseAddBags = do
  _ <- parseLiteral "AddBags"
  bags <- parseBags
  return $ AddBags bags

parseView :: Parser Query
parseView = do
  _ <- parseLiteral "View"
  return View

parseBrewBeer :: Parser Query
parseBrewBeer = do
  _ <- parseLiteral "BrewBeer"
  beer <- parseBeer
  return $ BrewBeer beer