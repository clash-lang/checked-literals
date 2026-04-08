{-# LANGUAGE QuasiQuotes #-}

module Tests.RequiredTypeArguments (tests) where

import Data.String.Interpolate (__i)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.AssertGhc (Expected (..), testCaseGhcWithFlags)

tests :: TestTree
tests =
  testGroup
    "RequiredTypeArguments"
    [ testCaseGhcWithFlags
        requiredTypeArgumentFlags
        "OK , required type argument literal"
        [__i|
          import Prelude
          import Data.Proxy
          import Data.Word
          import GHC.TypeNats

          f :: forall (n :: Nat) -> Proxy n
          f (type n) = Proxy

          x = f 7

          y :: Word8
          y = 255
        |]
        ExpectSuccess
    , testCaseGhcWithFlags
        requiredTypeArgumentFlags
        "NOK, required type argument literal with overflowing term literal"
        [__i|
          import Prelude
          import Data.Proxy
          import Data.Word
          import GHC.TypeNats

          f :: forall (n :: Nat) -> Proxy n
          f (type n) = Proxy

          x = f 7

          y :: Word8
          y = 256
        |]
        (ExpectFailure ["Literal 256 is out of bounds.", "Word8 has bounds: [0 .. 255]"])
    ]
 where
  requiredTypeArgumentFlags =
    [ "-XExplicitNamespaces"
    , "-XRequiredTypeArguments"
    ]
