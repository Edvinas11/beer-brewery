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
    "ingredients",
    "ingredient",
    "recipe",
    "process",
    "equipment",

    -- Types of Beer
    "Pale Ale",
    "Stout",
    "IPA",
    "Lager",
    "Ale",
    "Pilsner",

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

    -- Equipment
    "Mash Tun",
    "Kettle",
    "Fermenter",
    "Conditioning Tank",

    -- Commands
    "brewBeer",
    "addIngredient",
    "ferment",
    "condition"
    ]
