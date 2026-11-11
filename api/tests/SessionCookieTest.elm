module SessionCookieTest exposing (suite)

import Data.Origin as Origin
import Data.SessionToken as SessionToken
import Dict
import Expect
import Json.Encode as Encode
import Request exposing (Request)
import Response
import SessionCookie
import Test exposing (Test, describe, test)


requestWithCookie : String -> Request
requestWithCookie value =
    { method = "GET"
    , path = []
    , query = Dict.empty
    , headers = Dict.fromList [ ( "cookie", value ) ]
    , body = ""
    }


setHeader : String -> Maybe (Maybe ( String, String ))
setHeader origin =
    Maybe.map2
        (\o t ->
            Response.json 200 Encode.null
                |> SessionCookie.set o t
                |> .headers
                |> List.head
        )
        (Origin.parse origin |> Result.toMaybe)
        (SessionToken.fromString "c2Vzc2lvbg")


suite : Test
suite =
    describe "SessionCookie"
        [ test "finds the session among other cookies" <|
            \() ->
                requestWithCookie "theme=dark; session=c2Vzc2lvbg; other=1"
                    |> SessionCookie.read
                    |> Maybe.map SessionToken.toString
                    |> Expect.equal (Just "c2Vzc2lvbg")
        , test "finds no session in an empty cookie" <|
            \() ->
                requestWithCookie "session="
                    |> SessionCookie.read
                    |> Maybe.map SessionToken.toString
                    |> Expect.equal Nothing
        , test "sets a secure cookie over https" <|
            \() ->
                setHeader "https://pixo.pics"
                    |> Expect.equal
                        (Just (Just ( "set-cookie", "session=c2Vzc2lvbg; Path=/; Max-Age=2592000; HttpOnly; SameSite=Lax; Secure" )))
        , test "leaves Secure out over http" <|
            \() ->
                setHeader "http://localhost:5173"
                    |> Expect.equal
                        (Just (Just ( "set-cookie", "session=c2Vzc2lvbg; Path=/; Max-Age=2592000; HttpOnly; SameSite=Lax" )))
        , test "clears the cookie" <|
            \() ->
                Origin.parse "https://pixo.pics"
                    |> Result.toMaybe
                    |> Maybe.map
                        (\origin ->
                            Response.json 200 Encode.null
                                |> SessionCookie.clear origin
                                |> .headers
                                |> List.head
                        )
                    |> Expect.equal
                        (Just (Just ( "set-cookie", "session=; Path=/; Max-Age=0; HttpOnly; SameSite=Lax; Secure" )))
        ]
