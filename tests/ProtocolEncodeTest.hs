{-# LANGUAGE OverloadedStrings, QuasiQuotes, TemplateHaskell #-}
module ProtocolEncodeTest where

import Data.ByteString.Builder (toLazyByteString)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

import qualified Data.ByteString.Lazy as BSL

import qualified Imvu.Network.IstatdClient as Istatd

case_encode_increment :: Assertion
case_encode_increment = assertEqual
    "encoded increment directives have the form '*name value'"
    "*herfderf 1234\n"
    (BSL.toStrict . toLazyByteString . Istatd.encode $
        Istatd.Packet Istatd.Increment "herfderf" [] 1234)

case_encode_record :: Assertion
case_encode_record = assertEqual
    "encoded record directives have the form 'name value'"
    "herfderf 4321\n"
    (BSL.toStrict . toLazyByteString . Istatd.encode $
        Istatd.Packet Istatd.Record "herfderf" [] 4321)

case_encode_record_with_floating_value_test :: Assertion
case_encode_record_with_floating_value_test = assertEqual
    "encoded record directives with a floating point value have the form 'name value.fracpart'"
    "herfderf 2345.84332\n"
    (BSL.toStrict . toLazyByteString . Istatd.encode $
        Istatd.Packet Istatd.Record "herfderf" [] 2345.84332)

case_encode_with_suffix :: Assertion
case_encode_with_suffix = assertEqual
    "encoded directives with suffixes have the form 'name^suffix^suffix value'"
    "herfderf^hoober.huoh^doober 4321\n"
    (BSL.toStrict . toLazyByteString . Istatd.encode $
        Istatd.Packet Istatd.Record "herfderf" ["hoober.huoh", "doober"] 4321)

tests :: TestTree
tests = $(testGroupGenerator)

main :: IO ()
main = defaultMain tests
