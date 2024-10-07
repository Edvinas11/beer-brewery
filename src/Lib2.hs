module Lib2
    ( Query(..),
    parseQuery,
    State(..),
    emptyState,
    stateTransition,
    parseRecipe,
    parseTime
    ) where

import Debug.Trace (trace)

-- Helper to skip leading whitespaces
skipWhitespace :: String -> String
skipWhitespace = dropWhile (`elem` " \t\n")

-- Data Types based on your BNF

-- Represents a beer with a name, type, alcohol content, and ingredients
data Beer = Beer String String String [String]
  deriving (Eq, Show)

-- Represents a recipe with type, ingredients, and process
data Recipe = Recipe String [String] Process
  deriving (Eq, Show)

-- Represents a brewing process with mash, boil, ferment, and condition times
data Process = Process String String String String
  deriving (Eq, Show)

-- Different types of queries or commands the user can issue
data Query 
  = BrewBeer Recipe
  | AddIngredient String Recipe
  | Ferment Beer String
  | Condition Beer String
  deriving (Eq, Show)

-- Represents the state of the system, which stores a list of beers
data State = State [Beer]
  deriving (Eq, Show)

-- Creates the initial empty state
emptyState :: State
emptyState = State []

-- | Parses user input and returns a query based on the input
parseQuery :: String -> Either String Query
parseQuery input = 
    let trimmedInput = skipWhitespace input
    in if trimmedInput == "brewBeer"
       then Right (BrewBeer (Recipe "" [] (Process "" "" "" ""))) -- Return "brewBeer" without parsing a recipe
       else if take 8 trimmedInput == "brewBeer"
            then case parseBrewCommand (drop 8 trimmedInput) of
                   Just (query, "") -> Right query
                   _ -> Left "Parse error: Could not parse recipe."
            else Left "Parse error: Invalid command"

-- | Handles state transitions based on the parsed query
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition (State beers) (BrewBeer recipe) = 
  let newBeer = Beer (show recipe) "Ale" "5%" []  -- Simplified beer based on the recipe
      newState = State (newBeer : beers)
  in Right (Just "Beer brewed successfully!", newState)

stateTransition (State beers) (Ferment beer time) = 
  Right (Just ("Fermenting beer for " ++ time), State beers)

stateTransition (State beers) (Condition beer time) = 
  Right (Just ("Conditioning beer for " ++ time), State beers)

stateTransition (State beers) (AddIngredient ingredient recipe) = 
  Right (Just ("Added " ++ ingredient ++ " to the recipe"), State beers)

-- Parsers

-- <brew_command> ::= "brewBeer" <recipe>
parseBrewCommand :: String -> Maybe (Query, String)
parseBrewCommand input = 
  let rest = skipWhitespace input
  in case parseRecipe rest of
       Just (recipe, rest1) -> Just (BrewBeer recipe, skipWhitespace rest1)
       Nothing -> Nothing

-- <recipe> ::= "(" <type> <ingredients> <process> ")"
parseRecipe :: String -> Maybe (Recipe, String)
parseRecipe input = 
  let rest = skipWhitespace input
  in trace ("Parsing recipe: " ++ rest) $
     case rest of
      ('(':rest1) -> trace "Found opening paren for recipe" $
                     case parseType (skipWhitespace rest1) of
                       Just (typ, rest2) -> trace ("Parsed type: " ++ typ) $
                         case parseIngredients (skipWhitespace rest2) of
                           Just (ingredients, rest3) -> trace ("Parsed ingredients: " ++ show ingredients) $
                             case parseProcess (skipWhitespace rest3) of
                               Just (process, rest4) -> trace ("Parsed process: " ++ show process) $
                                 case skipWhitespace rest4 of
                                   (')':rest5) -> Just (Recipe typ ingredients process, rest5)
                                   _ -> trace "Failed to find closing paren for recipe" Nothing
                               _ -> trace "Failed to parse process" Nothing
                           _ -> trace "Failed to parse ingredients" Nothing
                       _ -> trace "Failed to parse type" Nothing
      _ -> trace "Failed to find opening paren for recipe" Nothing

-- <type> ::= "Lager" | "Ale" | "Stout"
parseType :: String -> Maybe (String, String)
parseType input = 
  let rest = skipWhitespace input
  in if take 5 rest == "Lager"
     then Just ("Lager", drop 5 rest)
     else if take 3 rest == "Ale"
     then Just ("Ale", drop 3 rest)
     else if take 5 rest == "Stout"
     then Just ("Stout", drop 5 rest)
     else Nothing

-- Helper function to parse multiple ingredients recursively
parseRestIngredients :: [String] -> String -> Maybe ([String], String)
parseRestIngredients acc input = 
  let rest = skipWhitespace input
  in case rest of
       (')':rest1) -> Just (reverse acc, rest1)  -- End of ingredients list
       _           -> case parseIngredient rest of
                        Just (ingredient, rest2) -> 
                          case skipWhitespace rest2 of
                            (')':rest3) -> parseRestIngredients (ingredient:acc) (skipWhitespace rest3)
                            _ -> Nothing  -- failed to find the closing parenthesis for ingredient
                        Nothing -> Nothing  -- Failed to parse an ingredient

-- <ingredients> ::= "(" <ingredient>+ ")"
parseIngredients :: String -> Maybe ([String], String)
parseIngredients input =
  let rest = skipWhitespace input
  in case rest of
       ('(':rest1) -> parseIngredientList [] (skipWhitespace rest1)
       _ -> Nothing

-- Helper function to parse a list of ingredients inside a single set of parentheses
parseIngredientList :: [String] -> String -> Maybe ([String], String)
parseIngredientList acc input =
  let rest = skipWhitespace input
  in case rest of
       (')':rest1) -> Just (reverse acc, rest1)  -- Closing parenthesis, return list
       _           -> case parseSingleIngredient rest of
                        Just (ingredient, rest2) -> parseIngredientList (ingredient:acc) rest2
                        Nothing -> Nothing  -- Failed to parse an ingredient

-- <ingredient> ::= "(" "Malt" | "Hops" | "Yeast" | "Water" ")"
parseIngredient :: String -> Maybe (String, String)
parseIngredient input =
  let rest = skipWhitespace input
  in case rest of
       ('(':r1) -> if take 4 r1 == "Malt" && r1 !! 4 == ')'
                   then Just ("Malt", drop 5 r1)
                   else if take 4 r1 == "Hops" && r1 !! 4 == ')'
                   then Just ("Hops", drop 5 r1)
                   else if take 5 r1 == "Yeast" && r1 !! 5 == ')'
                   then Just ("Yeast", drop 6 r1)
                   else if take 5 r1 == "Water" && r1 !! 5 == ')'
                   then Just ("Water", drop 6 r1)
                   else Nothing
       _ -> Nothing

-- Function to parse a single ingredient
parseSingleIngredient :: String -> Maybe (String, String)
parseSingleIngredient input =
  let rest = skipWhitespace input
  in if take 4 rest == "Malt"
     then Just ("Malt", drop 4 rest)
     else if take 4 rest == "Hops"
     then Just ("Hops", drop 4 rest)
     else if take 5 rest == "Yeast"
     then Just ("Yeast", drop 5 rest)
     else if take 5 rest == "Water"
     then Just ("Water", drop 5 rest)
     else Nothing

-- <process> ::= "(" "Mash" <time> "Boil" <time> "Ferment" <time> "Condition" <time> ")"
parseProcess :: String -> Maybe (Process, String)
parseProcess input = 
  let rest = skipWhitespace input
  in case rest of
       ('(':r1) -> do
         -- Parse "Mash" stage
         r2 <- parseStage "Mash" r1
         (mashTime, r3) <- parseTime (skipWhitespace r2)
         
         -- Parse "Boil" stage
         r4 <- parseStage "Boil" r3
         (boilTime, r5) <- parseTime (skipWhitespace r4)
         
         -- Parse "Ferment" stage
         r6 <- parseStage "Ferment" r5
         (fermentTime, r7) <- parseTime (skipWhitespace r6)
         
         -- Parse "Condition" stage
         r8 <- parseStage "Condition" r7
         (conditionTime, r9) <- parseTime (skipWhitespace r8)
         
         -- Ensure the process ends with ")"
         case skipWhitespace r9 of
           (')':restFinal) -> Just (Process mashTime boilTime fermentTime conditionTime, restFinal)
           _ -> Nothing
       _ -> Nothing

-- Helper function to parse a specific stage (e.g., "Mash", "Boil", "Ferment", "Condition")
parseStage :: String -> String -> Maybe String
parseStage stageName input =
  let rest = skipWhitespace input
  in if take (length stageName) rest == stageName
     then Just (drop (length stageName) rest)
     else Nothing

-- <time> ::= <number> <unit>
-- <number> ::= "1" | "2" | ... | "9"
-- <unit> ::= "minutes" | "hour" | "hours" | "weeks" | "month" | "months"
parseTime :: String -> Maybe (String, String)
parseTime input =
  let rest = skipWhitespace input
  in case span (`elem` ['0'..'9']) rest of
       ([], _) -> Nothing  -- No number found
       (num, rest1) -> 
         let rest2 = skipWhitespace rest1
         in if take 7 rest2 == "minutes"
            then Just (num ++ " minutes", drop 7 rest2)
            else if take 4 rest2 == "hour"
            then Just (num ++ " hour", drop 4 rest2)
            else if take 5 rest2 == "hours"
            then Just (num ++ " hours", drop 5 rest2)
            else if take 5 rest2 == "weeks"
            then Just (num ++ " weeks", drop 5 rest2)
            else if take 5 rest2 == "month"
            then Just (num ++ " month", drop 5 rest2)
            else if take 6 rest2 == "months"
            then Just (num ++ " months", drop 6 rest2)
            else Nothing  -- Invalid unit


