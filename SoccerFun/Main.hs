module Main where

import SoccerFun.UI.GL
import Children.Team as Children
import CatHazCheezburger.Team as CatHazCheezburger

main :: IO ()
main = runMatch CatHazCheezburger.team Children.team