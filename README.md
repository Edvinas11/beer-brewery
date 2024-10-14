# fp-2024

# Beer Brewery Domain

## Overview
This project models a beer brewery, focusing on the process of brewing various types of beer.

## BNF
Check ```grammar.txt``` for more info

## BNF for Bags (Graph implementation)
```
<bags> ::= "(" <bag> ")" | "(" <bag> <bags> ")"
<bag> ::= <ingredients> | "(" <bags> <ingredients> ")"
```

```<bags>```: represents one or more bags of ingredients.
    - Bags can be nested recursively, meaning a bag can contain other bags.

**Example 1**: ```((Water Hops))``` - Single bag with ingredients

**Example 2**: ```((Water Hops) ((Water Hops)))``` - bag with ingredients and bag


