{-# LANGUAGE InstanceSigs #-}
module Lib3
    ( stateTransition,
    StorageOp (..),
    storageOpLoop,
    parseCommand,
    parseStatements,
    marshallState,
    renderStatements,
    renderQuery,
    Statements (..),
    Command (..)
    ) where

import Control.Applicative (Alternative (many), (<|>))
import Control.Concurrent (Chan, newChan, readChan, writeChan)
import Control.Concurrent.STM (STM, TVar, atomically, readTVar, readTVarIO, writeTVar)
import Control.Monad (forever)
import Data.List (intercalate, partition)
import Data.Maybe (fromJust, isNothing)
import qualified Lib2
import Parsers
import System.Directory (doesFileExist)

data StorageOp = Save String (Chan ()) | Load (Chan (Maybe String))


-- | This function is started from main
-- in a dedicated thread. It must be used to control
-- file access in a synchronized manner: read requests
-- from chan, do the IO operations needed and respond
-- to a channel provided in a request.
-- Modify as needed.
storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop opChan = forever $ do
  op <- readChan opChan
  case op of
    Save s chan -> do
      writeFile "state.txt" s
      writeChan chan ()
    Load chan -> do
      exists <- doesFileExist "state.txt"
      if exists
        then do
          s' <- readFile "state.txt"
          writeChan chan $ Just s'
        else writeChan chan Nothing

data Statements = Batch [Lib2.Query] |
               Single Lib2.Query
               deriving (Eq)

data Command = StatementCommand Statements |
               LoadCommand |
               SaveCommand
               deriving (Show, Eq)

instance Show Statements where
  show :: Statements -> String
  show (Single q) = show q
  show (Batch qs) = "BEGIN\n" ++ concatMap ((++ ";\n") . renderQuery) qs ++ "END\n"

-- | Parses user's input.
parseCommand :: String -> Either String (Command, String)
parseCommand = parse (StatementCommand <$> statements <|> parseLoad <|> parseSave)

parseLoad :: Parser Command
parseLoad = do
  _ <- parseLiteral "load"
  return LoadCommand

parseSave :: Parser Command
parseSave = do
  _ <- parseLiteral "save"
  return SaveCommand

-- | Parses Statement.
-- Must be used in parseCommand.
-- Reuse Lib2 as much as you can.
-- You can change Lib2.parseQuery signature if needed.
parseStatements :: String -> Either String (Statements, String)
parseStatements = parse statements

-- | Converts program's state into Statements
-- (probably a batch, but might be a single query)
marshallState :: Lib2.State -> Statements
marshallState state = Batch queries
  where
    addIngredientsQueries = map (\ingredient -> AddIngredients [ingredient]) (Lib2.ingredientsStock state)
    brewBeerQueries = map (\beer -> BrewBeer beer) (Lib2.inventory state)
    queries = addIngredientsQueries ++ brewBeerQueries

renderQuery :: Lib2.Query -> String
renderQuery (Lib2.AddIngredients ingredients) =
  "AddIngredients(" ++ intercalate " " (map show ingredients) ++ ")"
renderQuery (Lib2.AddBags bags) = 
  "AddBags(" ++ show bags ++ ")"
renderQuery (Lib2.View) = 
  "View"
renderQuery (Lib2.BrewBeer beer) = 
  "BrewBeer(" ++ renderBeer beer ++ ")"

renderBeer :: Lib2.Beer -> String
renderBeer (Lib2.Beer name bType (Lib2.AlcoholContent percent _) ingr) =
  show name ++ " " ++ show bType ++ " " ++ show percent ++ "%" ++ " (" ++ unwords (map show ingr) ++ ")"

-- | Renders Statements into a String which
-- can be parsed back into Statements by parseStatements
-- function. The String returned by this function must be used
-- as persist program's state in a file. 
-- Must have a property test
-- for all s: parseStatements (renderStatements s) == Right(s, "")
renderStatements :: Statements -> String
renderStatements (Single q) = renderQuery q
renderStatements (Batch qs) = "BEGIN\n" ++ concatMap ((++ ";\n") . renderQuery) qs ++ "END\n"

-- | Updates a state according to a command.
-- Performs file IO via ioChan if needed.
-- This allows your program to share the state
-- between repl iterations, save the state to a file,
-- load the state from the file so the state is preserved
-- between program restarts.
-- Keep IO as small as possible.
-- State update must be executed atomically (STM).
-- Right contains an optional message to print, updated state
-- is stored in transactinal variable
stateTransition :: TVar Lib2.State -> Command -> Chan StorageOp -> IO (Either String (Maybe String))
stateTransition s SaveCommand ioChan = do
  s' <- readTVarIO s
  chan <- newChan :: IO (Chan ())
  writeChan ioChan (Save (renderStatements $ marshallState s') chan)
  readChan chan
  return $ Right $ Just "State saved successfully"
stateTransition s LoadCommand ioChan = do
  chan <- newChan :: IO (Chan (Maybe String))
  writeChan ioChan (Load chan)
  qs <- readChan chan
  if isNothing qs
    then return (Left "No state file found")
    else case parseStatements $ fromJust qs of
      Left e -> do
        return $ Left $ "Failed to load state from file:\n" ++ e
      Right (qs', _) -> atomically $ atomicStatements s qs'
stateTransition s (StatementCommand sts) _ = atomically $ atomicStatements s sts

transitionThroughList :: Lib2.State -> [Lib2.Query] -> Either String (Maybe String, Lib2.State)
transitionThroughList state queries =
  let (addIngredients, otherQueries) = partition isAddIngredients queries
      orderedQueries = addIngredients ++ otherQueries
  in foldl processQuery (Right (Nothing, state)) orderedQueries
  where
    processQuery :: Either String (Maybe String, Lib2.State) -> Lib2.Query -> Either String (Maybe String, Lib2.State)
    processQuery (Left err) _ = Left err
    processQuery (Right (accMsg, currentState)) query =
      case Lib2.stateTransition currentState query of
        Left err -> Left err
        Right (msg, newState) -> Right (Lib2.combineMessages accMsg msg, newState)

    isAddIngredients :: Lib2.Query -> Bool
    isAddIngredients (Lib2.AddIngredients _) = True
    isAddIngredients _ = False

atomicStatements :: TVar Lib2.State -> Statements -> STM (Either String (Maybe String))
atomicStatements s (Batch qs) = do
  s' <- readTVar s
  case transitionThroughList s' qs of
    Left e -> return $ Left e
    Right (msg, ns) -> do
      writeTVar s ns
      return $ Right msg
atomicStatements s (Single q) = do
  s' <- readTVar s
  case Lib2.stateTransition s' q of
    Left e -> return $ Left e
    Right (msg, ns) -> do
      writeTVar s ns
      return $ Right msg

statements :: Parser Statements
statements =
  ( do
      _ <- parseLiteral "BEGIN"
      _ <- parseLiteral "\n"
      q <-
        many
          ( do
              q <- Lib2.parseTask
              _ <- parseLiteral ";"
              _ <- parseLiteral "\n"
              return q
          )
      _ <- parseLiteral "END"
      _ <- parseLiteral "\n"
      return $ Batch q
  )
    <|> (Single <$> Lib2.parseTask)

