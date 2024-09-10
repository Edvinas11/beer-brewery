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
    - type: The type of beer being brewed.
    - Ingredients: Ingredients needed for the recipe.
    - Process: The steps involved in brewing the beer.

4. **Process**: Represents the individual steps for brewing a beer:
    - Mash: The first step where grains are mashed for a specific time.
    - Boil: Boiling the ingredients for a specific duration.
    - Ferment: The beer is left to ferment for a specific time.
    - Condition: The beer is conditioned or aged for a specific time.

## Recursive Elements
- **Ingredients**: This allows to specify multiple ingredients for a beer by recursively defining an ingredient list.
### Example
Let's create a Lager with Malt, Hops, and Yeast as ingredients
```plain text
BNF representation
<ingredients> ::= "Malt" <ingredients>
<ingredients> ::= "Hops" <ingredients>
<ingredients> ::= "Yeast"
```
```plain text
brewBeer Lager Malt Hops Yeast Mash 30 minutes Boil 1 hour Ferment 2 weeks Condition 1 month
```

## Symbol Definitions
1. ```<beer>```: Represents a beer object.
    - **Example**: ```Guinness Stout 6% Malt Hops Yeast Water```

2. ```<name>```: Specifies the name of the beer.

3. ```<type>```: Describes the type of beer.

4. ```<alcohol-content>```: Describes the percentage of alcohol in the beer.

5. ```<percentage>```: Specifies the alcohol percentage in the beer.

6. ```<ingredients>```: A list of ingredients used in the beer, defined recursively. It can either be a single ```<ingredient>``` or multiple ```<ingredient>``` elements.

7.  ```<ingredient>```: A single ingredient used in the brewing process.

8. ```<recipe>```: Describes the entire brewing process for a beer, including the type of beer, the ingredients, and the process steps (mashing, boiling, fermenting, conditioning).
    - **Example**: ```Ale Malt Hops Yeast Mash 30 minutes Boil 1 hour Ferment 2 weeks Condition 1 month```

9. ```<process>```: Outlines the steps in the brewing process.

10. ```<time>```: Specifies the duration for each brewing process step.

11. ```<time-period>```: Describes the time required for fermenting or conditioning a beer.

12. ```<number>```: Integer number.

## Operations

1. ```brewBeer```: This operation initiates the brewing process for a beer based on a specified recipe.
    - **Syntax**: ```brewBeer <recipe>```
    - **Example**: ```brewBeer Ale Malt Hops Yeast Mash 30 minutes Boil 1 hour Ferment 2 weeks Condition 1 month```

2. ```addIngredient```: This operation adds an ingredient to a given recipe.
    - **Syntax**: ```addIngredient <ingredient> <recipe>```
    - **Example**: ```addIngredient Hops Stout Malt Hops Yeast Mash 1 hour Boil 1 hour Ferment 2 weeks Condition 1 month```

3. ```ferment```: This operation ferments the beer for a specified time period.
    - **Syntax**: ```ferment <beer> <time-period>```
    - **Example**: ```ferment Pale Ale Lager 5% Malt Hops Yeast Water Mash 30 minutes Boil 1 hour Ferment 2 weeks Condition 1 month 2 weeks```

4. ```condition```: Conditions a specified beer for a given time period, allowing it to mature.
    - **Syntax**: condition ```<beer> <time-period>```
    - **Example**: ```condition Guinness Stout 6% Malt Hops Yeast Water Mash 1 hour Boil 1 hour Ferment 2 weeks Condition 1 month 1 month```