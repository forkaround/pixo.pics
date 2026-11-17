module HmacTest exposing (suite)

import Expect
import Hmac
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Hmac"
        [ describe "sha256"
            [ test "RFC 4231 case 1" <|
                \() ->
                    Hmac.toHex (Hmac.sha256 (List.repeat 20 0x0B) (Hmac.utf8 "Hi There"))
                        |> Expect.equal "b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7"
            , test "RFC 4231 case 2" <|
                \() ->
                    Hmac.toHex (Hmac.sha256 (Hmac.utf8 "Jefe") (Hmac.utf8 "what do ya want for nothing?"))
                        |> Expect.equal "5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843"
            , test "RFC 4231 case 6, a key longer than the block" <|
                \() ->
                    Hmac.toHex (Hmac.sha256 (List.repeat 131 0xAA) (Hmac.utf8 "Test Using Larger Than Block-Size Key - Hash Key First"))
                        |> Expect.equal "60e431591ee0b67f0d8a26aacbf5b77f8e0bc6213728c5140546040f0ee37f54"
            ]
        , test "utf8 gives the bytes of a multi-byte character" <|
            \() ->
                Hmac.utf8 "é"
                    |> Expect.equal [ 0xC3, 0xA9 ]
        , test "toHex writes two lower-case digits per byte" <|
            \() ->
                Hmac.toHex [ 0, 15, 255 ]
                    |> Expect.equal "000fff"
        ]
