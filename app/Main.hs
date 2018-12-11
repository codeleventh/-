module Main where

import System.Random

import HS00
import HS10
import HS20
import Hutton5

main :: IO ()
main = do
  print "try stack ghci hs99"
  return ()

p23_test :: IO ()
p23_test = do
  gen <- getStdGen
  let (l, gen') = p23 ['a'..'z'] 4 gen
  print l
  return ()
