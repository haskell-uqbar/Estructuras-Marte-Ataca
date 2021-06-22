module Spec where
import PdePreludat
import Marte
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Test de ejemplo" $ do
    it "La fiebre luego del covid no es mas potente que al reves" $ do
      combinacionMasPotente fiebreRoja covid tierra `shouldBe` False

