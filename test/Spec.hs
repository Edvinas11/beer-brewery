{-# LANGUAGE ImportQualifiedPost #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )
import Test.Tasty.QuickCheck as QC

import Data.List
import Data.Ord

import Lib1 qualified
import Lib2 qualified
import Lib3 qualified

import Test.QuickCheck (Arbitrary(..), elements)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, propertyTests]

unitTests :: TestTree
unitTests = testGroup "Lib2 tests"
  [ 
    testCase "AddIngredients command parsing with one ingredient" $
      Lib2.parseQuery "AddIngredients(Water)"
        @?= Right (Lib2.AddIngredients [Lib2.Water]),
        
    testCase "AddIngredients command parsing with multiple ingredients" $
      Lib2.parseQuery "AddIngredients(Water Hops Yeast)"
        @?= Right (Lib2.AddIngredients [Lib2.Water, Lib2.Hops, Lib2.Yeast]),

    testCase "AddBags command parsing with one bag of ingredients" $
      Lib2.parseQuery "AddBags((Water Hops Yeast))"
        @?= Right (Lib2.AddBags [Lib2.BagWithIngredients [Lib2.Water, Lib2.Hops, Lib2.Yeast]]),

    testCase "AddBags command parsing with multiple bags" $
      Lib2.parseQuery "AddBags((Water Hops) ((Water)))"
        @?= Right (Lib2.AddBags 
              [Lib2.BagWithIngredients [Lib2.Water, Lib2.Hops],
              Lib2.BagWithIngredients [Lib2.Water]]),

    testCase "AddBags command parsing with nested bags" $
      Lib2.parseQuery "AddBags((((Hops Hops) ((Yeast Hops Malt))) (Hops Water)))"
        @?= Right (Lib2.AddBags 
              [Lib2.BagWithBagsAndIngredients 
                [Lib2.BagWithIngredients [Lib2.Hops, Lib2.Hops],
                Lib2.BagWithIngredients [Lib2.Yeast, Lib2.Hops, Lib2.Malt]]
                [Lib2.Hops, Lib2.Water]]),

    testCase "BrewBeer command parsing with simple beer" $
      Lib2.parseQuery "BrewBeer(PaleAle Lager 5% (Malt Hops Yeast))"
        @?= Right (Lib2.BrewBeer (Lib2.Beer Lib2.PaleAle Lib2.Lager (Lib2.AlcoholContent 5 '%') [Lib2.Malt, Lib2.Hops, Lib2.Yeast])),

    testCase "stateTransition - Adding ingredients updates state" $
      let initialState = Lib2.emptyState
          query = Lib2.AddIngredients [Lib2.Water, Lib2.Hops]
          expectedState = Lib2.State { Lib2.inventory = [], Lib2.ingredientsStock = [Lib2.Water, Lib2.Hops] }
      in Lib2.stateTransition initialState query @?=
          Right (Just "Ingredients added: [Water,Hops]", expectedState)
  ]

instance Arbitrary Lib2.Query where
    arbitrary = oneof
      [ Lib2.AddIngredients <$> listOf1 arbitrary
      , Lib2.BrewBeer <$> arbitrary
      , return Lib2.View
      ]

instance Arbitrary Lib2.Beer where
    arbitrary = Lib2.Beer <$> arbitrary <*> arbitrary <*> arbitrary <*> (listOf1 arbitrary)

instance Arbitrary Lib2.BeerName where
    arbitrary = elements [Lib2.PaleAle, Lib2.Guinness]

instance Arbitrary Lib2.BeerType where
    arbitrary = elements [Lib2.Lager, Lib2.Ale, Lib2.Stout]

instance Arbitrary Lib2.AlcoholContent where
    arbitrary = Lib2.AlcoholContent <$> choose (0, 15) <*> return '%'

instance Arbitrary Lib2.Ingredient where
    arbitrary = elements [Lib2.Malt, Lib2.Hops, Lib2.Yeast, Lib2.Water]

propertyTests :: TestTree
propertyTests = testGroup "Property tests"
  [ QC.testProperty "sort is idempotent" $
      \list -> sort (sort (list :: [Int])) == sort list,
    QC.testProperty "stateTransition preserves stock when brewing beer" $
      \ingredients ->
        let initialState = Lib2.State { Lib2.inventory = [], Lib2.ingredientsStock = ingredients }
            beer = Lib2.Beer Lib2.PaleAle Lib2.Ale (Lib2.AlcoholContent 5 '%') ingredients
            query = Lib2.BrewBeer beer
            result = Lib2.stateTransition initialState query
        in case result of
             Right (_, finalState) -> Lib2.ingredientsStock finalState == Lib2.ingredientsStock initialState
             _ -> False,
    QC.testProperty "parseQuery . renderQuery == Right query" $
        \query ->
          Lib2.parseQuery (Lib3.renderQuery query) == Right query
  ]

