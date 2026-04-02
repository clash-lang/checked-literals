{-# LANGUAGE QuasiQuotes #-}

module Tests.Common where

import Data.String.Interpolate (__i)
import Test.Tasty (TestTree)
import Test.Tasty.AssertGhc (Expected (..), testCaseGhc)

toTestCases :: [(String, String, String, [String])] -> [TestTree]
toTestCases = map toTestCase

toTestCase :: (String, String, String, [String]) -> TestTree
toTestCase (moduleName, typeName, literal, expectedErrors) =
  testCaseGhc
    ((if null expectedErrors then "OK , " else "NOK, ") ++ typeName ++ ", " ++ literal)
    [__i|
      import Prelude
      import #{moduleName}
      import GHC.TypeNats
      test :: #{typeName}
      test = #{literal}
    |]
    ( if null expectedErrors
        then ExpectSuccess
        else ExpectFailure expectedErrors
    )

toCaseTestCases :: [(String, String, String, [String])] -> [TestTree]
toCaseTestCases = map toCaseTestCase

toCaseTestCase :: (String, String, String, [String]) -> TestTree
toCaseTestCase (moduleName, typeName, literal, expectedErrors) =
  testCaseGhc
    ((if null expectedErrors then "OK , " else "NOK, ") ++ typeName ++ ", case " ++ literal)
    [__i|
      import Prelude
      import #{moduleName}
      import GHC.TypeNats
      import SafeLiterals
      test :: #{typeName}
      test = case 0 of
        (#{literal} :: (#{typeName})) -> 0
        _ -> 0
    |]
    ( if null expectedErrors
        then ExpectSuccess
        else ExpectFailure expectedErrors
    )
