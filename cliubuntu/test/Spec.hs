{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import qualified Test.Tasty.HUnit as H
import qualified Test.Tasty.Hedgehog as E
import qualified Update as U
import qualified Lang
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString as B
import qualified Data.Map as M


main :: IO ()
main =
    defaultMain $ testGroup "Tests" [lang, bwt]


helloWorldTl :: B.ByteString
helloWorldTl = 
    encodeUtf8
    "f\n\
    \..msg model\n\
    \.\n\
    \..\"print\" \"hello world\"\n"


lang :: TestTree
lang =
    testGroup "language tests"
    [ H.testCase "hello world" $
        case Lang.compile helloWorldTl of
        Left err ->
            H.assertFailure $ mconcat 
            [ "expecting successful compile, but got: "
            , show err
            ]
        Right f ->
            f Lang.Start M.empty H.@?= (M.empty, Lang.Print "hello world")
    ]


bwt :: TestTree
bwt =
    testGroup "BigWebThing tests"
    [ let
        got = U.update U.initModel U.Start
        expected = (U.initModel, U.Cmds [U.GetArgs, U.SetupDb])
      in
        H.testCase "Start" $ got H.@?= expected
    ]
