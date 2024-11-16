{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Lib2
    ( Query(..),
    parseQuery,
    State(..),
    emptyState,
    parseTask,
    stateTransition,
    ) where

import qualified Data.Char as C
import qualified Data.List as L

import Parsers

-- | An entity which represents your program's state.
data State = State
  {
    inventory :: [Beer],
    ingredientsStock :: [Ingredient]
  }
  deriving (Show, Eq)

-- | Creates an initial program's state.
emptyState :: State
emptyState = State {inventory = [], ingredientsStock = []}

-- | Parses user's input.
parseQuery :: String -> Either String Query
parseQuery input = 
  case parse parseTaskList input of
    Left e -> Left e
    Right (qs, r) -> if null r
      then case qs of
        [q] -> Right q
        _ -> Right (Sequence qs)
      else Left ("Unrecognized characters: " ++ r)

-- | Updates a state according to a query.
stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition st query = case query of
  AddIngredients ingredients ->
    let newState = st {ingredientsStock = ingredients ++ ingredientsStock st}
     in Right (Just $ "Ingredients added: " ++ show ingredients, newState)
  AddBags bags ->
    let ingredientsFromBags = concatMap extractIngredientsFromBag bags
        newState = st {ingredientsStock = ingredientsFromBags ++ ingredientsStock st}
     in Right (Just $ "Ingredients added from bags: " ++ show ingredientsFromBags, newState) 
  View ->
    let inventoryStr = if null (inventory st)
                     then "No beers in inventory."
                     else unlines (map showBeer (inventory st))
        ingredientsStr = if null (ingredientsStock st)
                       then "No ingredients in stock."
                       else unlines (map show (ingredientsStock st))
     in Right (Just $ "Inventory:\n" ++ inventoryStr ++ "\nIngredients:\n" ++ ingredientsStr, st)
  BrewBeer beer ->
    let beerIngredients = ingredients beer
        currentStock = ingredientsStock st
    in case hasEnoughIngredients beerIngredients currentStock of
      Left err -> Left err -- not enough ingredients
      Right updatedStock ->
        let newState = st { inventory = beer : inventory st, ingredientsStock = updatedStock }
         in Right (Just $ "Brewed beer: " ++ show (beerName beer), newState)
  Sequence queryList ->
    foldl processQuery (Right (Just "", st)) queryList
    where
      processQuery :: Either String (Maybe String, State) -> Query -> Either String (Maybe String, State)
      processQuery (Left err) _ = Left err
      processQuery (Right (accMsg, currentState)) nextQuery =
        case stateTransition currentState nextQuery of
          Left err -> Left err
          Right (Just result, newState) ->
            Right (combineMessages accMsg (Just result), newState)
          Right (Nothing, newState) -> Right (accMsg, newState)

combineMessages :: Maybe String -> Maybe String -> Maybe String
combineMessages Nothing Nothing = Nothing
combineMessages (Just msg) Nothing = Just msg
combineMessages Nothing (Just msg) = Just msg
combineMessages (Just msg1) (Just msg2) = Just (msg1 ++ "\n" ++ msg2)

hasEnoughIngredients :: [Ingredient] -> [Ingredient] -> Either String [Ingredient]
hasEnoughIngredients [] stock = Right stock
hasEnoughIngredients (ingr:rest) stock =
  case removeIngredient ingr stock of
    Left err -> Left err  -- Not enough of this ingredient
    Right updatedStock -> hasEnoughIngredients rest updatedStock

removeIngredient :: Ingredient -> [Ingredient] -> Either String [Ingredient]
removeIngredient ingredient stock =
  case L.elemIndex ingredient stock of
    Nothing -> Left $ "Not enough " ++ show ingredient ++ " in stock."
    Just idx -> Right (take idx stock ++ drop (idx + 1) stock)

extractIngredientsFromBag :: Bag -> [Ingredient]
extractIngredientsFromBag (BagWithIngredients ingredients) = ingredients
extractIngredientsFromBag (BagWithBagsAndIngredients bags ingredients) =
  concatMap extractIngredientsFromBag bags ++ ingredients

showBeer :: Beer -> String
showBeer beer = "Beer: " ++ show (beerName beer) ++ ", Type: " ++ show (beerType beer) ++ 
                ", Alcohol: " ++ show (alcoholContent beer) ++ 
                ", Ingredients: " ++ show (ingredients beer)
