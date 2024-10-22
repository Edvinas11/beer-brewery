{-# LANGUAGE ImportQualifiedPost #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )

import Lib1 qualified
import Lib2 qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests :: TestTree
unitTests = testGroup "Lib2 tests"
  [ 
    testCase "AddIngredients command parsing with one ingredient" $
      Lib2.parseQuery "AddIngredients (Water)"
        @?= Right (Lib2.AddIngredients [Lib2.Water]),
        
    testCase "AddIngredients command parsing with multiple ingredients" $
      Lib2.parseQuery "AddIngredients (Water Hops Yeast)"
        @?= Right (Lib2.AddIngredients [Lib2.Water, Lib2.Hops, Lib2.Yeast]),

    testCase "AddBags command parsing with one bag of ingredients" $
      Lib2.parseQuery "AddBags ((Water Hops Yeast))"
        @?= Right (Lib2.AddBags [Lib2.BagWithIngredients [Lib2.Water, Lib2.Hops, Lib2.Yeast]]),

    testCase "AddBags command parsing with multiple bags" $
      Lib2.parseQuery "AddBags ((Water Hops) ((Water)))"
        @?= Right (Lib2.AddBags 
              [Lib2.BagWithIngredients [Lib2.Water, Lib2.Hops],
              Lib2.BagWithIngredients [Lib2.Water]]),

    testCase "AddBags command parsing with nested bags" $
      Lib2.parseQuery "AddBags ((((Hops Hops) ((Yeast Hops Malt))) (Hops Water)))"
        @?= Right (Lib2.AddBags 
              [Lib2.BagWithBagsAndIngredients 
                [Lib2.BagWithIngredients [Lib2.Hops, Lib2.Hops],
                Lib2.BagWithIngredients [Lib2.Yeast, Lib2.Hops, Lib2.Malt]]
                [Lib2.Hops, Lib2.Water]]),

    testCase "BrewBeer command parsing with simple beer" $
      Lib2.parseQuery "BrewBeer (PaleAle Lager 5% (Malt Hops Yeast))"
        @?= Right (Lib2.BrewBeer (Lib2.Beer Lib2.PaleAle Lib2.Lager (Lib2.AlcoholContent 5 '%') [Lib2.Malt, Lib2.Hops, Lib2.Yeast]))
  ]