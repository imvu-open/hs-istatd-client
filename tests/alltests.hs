module Main where

import Test.Tasty

import qualified ProtocolEncodeTest

main :: IO ()
main = defaultMain $ testGroup "alltests" [ ProtocolEncodeTest.tests ]
