# fp-2024

# Beer Brewery Domain

## Overview
This project models a beer brewery, focusing on the process of brewing various types of beer.

## Main Entities
1. **Beer**: Represents the final product. Each beer has:
    - Name: The name of the beer.
    - Type: The type of beer.
    - Alcohol Content: The percentage of alcohol.
    - Ingredients: List of ingredients used.

2. **Ingredient**: Components used in brewing:
    - Malt: Processed barley used in brewing.
    - Hops: Adds bitterness and flavor.
    - Yeast: For fermentation.
    - Water: The primary component of beer.

3. **Recipe**: A set of instructions for brewing beer, which can include:
    - Base Recipe: The fundamental recipe for a type of beer.
    - Ingredients: Ingredients needed for the recipe.
    - Processes: Steps like mashing, boiling, fermenting.

4. **Brewery Equipment**: Tools used in brewing:
    - Mash Tun: Converts malt into sugars.
    - Fermenter: Where fermentation occurs.
    - Kettle: For boiling the wort and adding hops.

## Recursive Elements
- **Recipe**: Can reference other recipes.
- **Ingredient Sourcing**: Some ingredients are produced from other ingredients.

## Operations
- **brewBeer**: Starts the brewing process for a specific type of beer using a recipe.
- **addIngredient**: Adds a specific ingredient to a beer recipe.
- **ferment**: Begins the fermentation process for a batch of beer, allowing yeast to convert sugars into alcohol.
- **condition**: Conditions the beer by allowing it to mature, enhancing its flavor and quality over time.
- **mash**: Converts malt starches into fermentable sugars in the brewing process.
- **store**: Stores the beer for aging or distribution after fermentation and conditioning.
- **filter**: Filters the beer to remove any unwanted particles, improving clarity and texture.