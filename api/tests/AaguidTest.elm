module AaguidTest exposing (suite)

import Aaguid
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Aaguid"
        [ test "names a passkey provider" <|
            \() ->
                Aaguid.name "fbfc3007-154e-4ecc-8c0b-6e020557d7bd"
                    |> Expect.equal (Just "Apple Passwords")
        , test "names nothing for an authenticator that hides its model" <|
            \() ->
                Aaguid.name "00000000-0000-0000-0000-000000000000"
                    |> Expect.equal Nothing
        ]
