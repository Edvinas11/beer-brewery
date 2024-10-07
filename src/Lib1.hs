module Lib1
    ( completions
    ) where

-- | This function returns a list of words
-- to be autocompleted in your program's repl.
completions :: [String]
completions = [
    -- Entities
    "beer",
    "name",
    "type",
    "alcohol-content",
    "percentage",
    "ingredients",
    "ingredient",
    "recipe",
    "process",
    "number",

    -- Types of Beer
    "Ale",
    "Stout",
    "Lager",

    -- Ingredients
    "Malt",
    "Hops",
    "Yeast",
    "Water",

    -- Processes
    "Mash",
    "Boil",
    "Ferment",
    "Condition",

    "time",

    -- Commands
    "brewBeer",
    "addIngredient",
    "ferment",
    "condition"
    ]
