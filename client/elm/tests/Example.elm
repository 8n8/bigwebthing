module Example exposing (suite)

import Expect
import Test
import Testable as T


suite : Test.Test
suite =
    Test.describe "Testable module"
        [ Test.describe "add"
            [ Test.test "2 + 2 = 5" <|
                \_ ->
                    Expect.equal (T.add 2 2) 4
            ]
        ]
