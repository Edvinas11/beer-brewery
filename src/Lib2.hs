{-# LANGUAGE InstanceSigs #-}
module Lib2
    ( Query(..),
    BrewCommand(..),
    AddIngredientCommand(..),
    FermentCommand(..),
    ConditionCommand(..),
    parseQuery,
    State(..),
    emptyState,
    stateTransition
    ) where

-- | An entity which represents user input.
data Query = Brew BrewCommand
           | Add AddIngredientCommand
           | Ferment FermentCommand
           | Condition ConditionCommand
           deriving (Eq, Show)

data Beer = Beer
  { beerName       :: String,
    beerType       :: String,
    alcoholContent :: String,
    beerIngredients    :: [Ingredient]
  } 
  deriving (Eq, Show)

data Ingredient = Malt | Hops | Yeast | Water
  deriving (Eq, Show)

data Recipe = Recipe 
  { recipeType       :: String, 
    recipeIngredients    :: [Ingredient], 
    process        :: Process
  } 
  deriving (Eq, Show)

data Process = Process 
  { mash       :: String, 
    boil       :: String, 
    ferment    :: String, 
    condition  :: String
  } 
  deriving (Eq, Show)

data Time = Minutes30 | Hour1 | Weeks2 | Month1 | Months6
  deriving (Eq, Show)

data BrewCommand = BrewCommand Recipe
  deriving (Eq, Show)

data AddIngredientCommand = AddIngredientCommand Ingredient Recipe
  deriving (Eq, Show)

data FermentCommand = FermentCommand Beer Time
  deriving (Eq, Show)

data ConditionCommand = ConditionCommand Beer Time
  deriving (Eq, Show)

-- | Parsing functions
-- <brew_command> ::= "brewBeer" <recipe>
parseBrewCommand :: String -> Either String BrewCommand
parseBrewCommand input = case words input of 
  ("brewBeer":rest) -> case parseRecipe (unwords rest) of
    Right recipe -> Right (BrewCommand recipe)
    Left err -> Left err
  _ -> Left "Invalid brewBeer command"

-- <add_ingredient_command> ::= "addIngredient" <ingredient> <recipe>
parseAddIngredientCommand :: String -> Either String AddIngredientCommand
parseAddIngredientCommand input = case words input of
    ("addIngredient":ingredientStr:rest) -> case parseIngredient ingredientStr of
        Right ingredient -> case parseRecipe (unwords rest) of
            Right recipe -> Right (AddIngredientCommand ingredient recipe)
            Left err -> Left err
        Left err -> Left err
    _ -> Left "Invalid addIngredient command"

-- <ferment_command> ::= "ferment" <beer> <time>
parseFermentCommand :: String -> Either String FermentCommand
parseFermentCommand input = case words input of
    ("ferment":rest) -> case parseBeerAndTime (unwords rest) of
        Right (beer, time) -> Right (FermentCommand beer time)
        Left err -> Left err
    _ -> Left "Invalid ferment command"

-- <condition_command> ::= "condition" <beer> <time>
parseConditionCommand :: String -> Either String ConditionCommand
parseConditionCommand input = case words input of
    ("condition":rest) -> case parseBeerAndTime (unwords rest) of
        Right (beer, time) -> Right (ConditionCommand beer time)
        Left err -> Left err
    _ -> Left "Invalid condition command"

-- Helper Parsing Functions

-- Parses a recipe according to the BNF
-- <recipe> ::= "(" <type> <ingredients> <process> ")"
parseRecipe :: String -> Either String Recipe
parseRecipe input = 
    -- Parsing logic for the recipe, ingredients, and process 
    Right (Recipe "Lager" [Malt, Hops, Yeast] (Process "Mash 1 hour" "Boil 1 hour" "Ferment 2 weeks" "Condition 1 month"))

-- Parses a beer according to the BNF
-- <beer> ::= "(" <name> <type> <alcohol_content> <ingredients> ")"
parseBeer :: String -> Either String Beer
parseBeer input = 
    -- Parsing logic for beer, with name, type, and alcohol content
    Right (Beer "Pale Ale" "Ale" "5%" [Malt, Hops])

-- Parses an ingredient
-- <ingredient> ::= "(" "Malt" | "Hops" | "Yeast" | "Water" ")"
parseIngredient :: String -> Either String Ingredient
parseIngredient "Malt" = Right Malt
parseIngredient "Hops" = Right Hops
parseIngredient "Yeast" = Right Yeast
parseIngredient "Water" = Right Water
parseIngredient _ = Left "Unknown ingredient"

-- Parses time values according to BNF
-- <time> ::= "30 minutes" | "1 hour" | "2 weeks" | "1 month" | "6 months"
parseTime :: String -> Either String Time
parseTime "30 minutes" = Right Minutes30
parseTime "1 hour" = Right Hour1
parseTime "2 weeks" = Right Weeks2
parseTime "1 month" = Right Month1
parseTime "6 months" = Right Months6
parseTime _ = Left "Invalid time format"

-- Parses a beer and time for ferment and condition commands
parseBeerAndTime :: String -> Either String (Beer, Time)
parseBeerAndTime input = 
    -- Assuming the beer and time parsing logic
    Right (Beer "Pale Ale" "Ale" "5%" [Malt, Hops], Weeks2)

-- | Parses user's input.
parseQuery :: String -> Either String Query
parseQuery input
    | "brewBeer" `elem` words input = case parseBrewCommand input of
        Right brewCommand -> Right (Brew brewCommand)
        Left err -> Left err
    | "addIngredient" `elem` words input = case parseAddIngredientCommand input of
        Right addCommand -> Right (Add addCommand)
        Left err -> Left err
    | "ferment" `elem` words input = case parseFermentCommand input of
        Right fermentCommand -> Right (Ferment fermentCommand)
        Left err -> Left err
    | "condition" `elem` words input = case parseConditionCommand input of
        Right conditionCommand -> Right (Condition conditionCommand)
        Left err -> Left err
    | otherwise = Left "Invalid command"

-- | An entity which represents your program's state.
data State = State
  { stateBeers       :: [String],   -- List of beers created
    stateRecipes     :: [Recipe],   
    stateIngredients :: [Ingredient]   -- Ingredients for the current recipe
  } deriving (Eq, Show)

-- | Creates an initial program's state.
emptyState :: State
emptyState = State [] [] []

-- | Updates a state according to a query.
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition state (Brew brewCommand) =
    stateTransitionBrew state brewCommand

stateTransition state (Add addCommand) =
    stateTransitionAdd state addCommand

stateTransition state (Ferment fermentCommand) =
    stateTransitionFerment state fermentCommand

stateTransition state (Condition conditionCommand) =
    stateTransitionCondition state conditionCommand

-- Individual state transition functions for each command

stateTransitionBrew :: State -> BrewCommand -> Either String (Maybe String, State)
stateTransitionBrew state (BrewCommand recipe) = 
    let newState = state { stateRecipes = recipe : stateRecipes state }
    in Right (Just "Beer brewed", newState)

stateTransitionAdd :: State -> AddIngredientCommand -> Either String (Maybe String, State)
stateTransitionAdd state (AddIngredientCommand ingredient recipe) = 
    let newState = state { stateIngredients = ingredient : stateIngredients state }
    in Right (Just "Ingredient added", newState)

stateTransitionFerment :: State -> FermentCommand -> Either String (Maybe String, State)
stateTransitionFerment state (FermentCommand beer time) = 
    Right (Just "Beer fermented", state)

stateTransitionCondition :: State -> ConditionCommand -> Either String (Maybe String, State)
stateTransitionCondition state (ConditionCommand beer time) = 
    Right (Just "Beer conditioned", state)
