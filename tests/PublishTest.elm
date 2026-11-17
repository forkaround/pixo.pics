module PublishTest exposing (suite)

import ApiClient
import Data.PictureId as PictureId exposing (PictureId)
import Data.Username as Username
import Dict
import Effect exposing (Effect(..))
import Expect
import Idb
import Json.Encode as Encode
import Library exposing (Event(..), Msg(..))
import Publish
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Publish"
        [ test "sends nothing for a second confirmation of a picture that is busy" <|
            \() ->
                let
                    ( busy, _ ) =
                        Publish.update library (Publish.PublishConfirmed 1) Publish.init
                in
                case Publish.update library (Publish.PublishConfirmed 1) busy of
                    ( again, NoEffect ) ->
                        Expect.equal (Publish.step 1 again) (Just Publish.Encoding)

                    _ ->
                        Expect.fail "Expected nothing to happen."
        , test "asks nothing for a picture the library does not hold" <|
            \() ->
                case Publish.update library (Publish.PublishConfirmed 9) Publish.init of
                    ( model, NoEffect ) ->
                        Expect.equal (Publish.step 9 model) Nothing

                    _ ->
                        Expect.fail "Expected nothing to happen."
        , test "marks the picture unpublished even when the server no longer knows it" <|
            \() ->
                Publish.outcome (Publish.GotUnpublished 1 (Err (ApiClient.Refused "No such picture.")))
                    |> Expect.equal (Publish.Marked 1 Nothing)
        , test "stops being busy when the server no longer knows the picture" <|
            \() ->
                case Publish.update library (Publish.GotUnpublished 1 (Err (ApiClient.Refused "No such picture."))) unpublishing of
                    ( model, Notify notice ) ->
                        ( Publish.step 1 model, notice.title )
                            |> Expect.equal ( Nothing, "Unpublished" )

                    _ ->
                        Expect.fail "Expected a notice."
        , test "cancels the unconfirmed row when the upload is refused" <|
            \() ->
                withRemote
                    (\remote ->
                        case Publish.update library (Publish.Sent 1 { status = 500, body = "" }) (uploading remote) of
                            ( model, Batch [ Request request, Notify notice ] ) ->
                                ( ( Publish.step 1 model, request.method ), request.url, notice.description )
                                    |> Expect.equal ( ( Nothing, "DELETE" ), "/api/pictures/" ++ remoteText, Just "The upload didn't go through." )

                            _ ->
                                Expect.fail "Expected a cancellation and a notice."
                    )
        , test "confirms the upload when the link took it" <|
            \() ->
                withRemote
                    (\remote ->
                        case Publish.update library (Publish.Sent 1 { status = 200, body = "" }) (uploading remote) of
                            ( model, Request request ) ->
                                ( Publish.step 1 model, request.method, request.url )
                                    |> Expect.equal ( Just (Publish.Confirming remote), "POST", "/api/pictures/" ++ remoteText ++ "/published" )

                            _ ->
                                Expect.fail "Expected the confirmation."
                    )
        , test "ignores an answer for an upload it never started" <|
            \() ->
                case Publish.update library (Publish.Sent 1 { status = 200, body = "" }) Publish.init of
                    ( _, NoEffect ) ->
                        Expect.pass

                    _ ->
                        Expect.fail "Expected nothing to happen."
        ]


remoteText : String
remoteText =
    "6f1c2a52-3d0e-4a53-9a7e-0f6f1f3f5b11"


withRemote : (PictureId -> Expect.Expectation) -> Expect.Expectation
withRemote toExpectation =
    case PictureId.fromString remoteText of
        Just remote ->
            toExpectation remote

        Nothing ->
            Expect.fail "The example id should parse."


uploading : PictureId -> Publish.Model
uploading remote =
    { confirming = Nothing, busy = Dict.fromList [ ( 1, Publish.Uploading remote ) ] }


unpublishing : Publish.Model
unpublishing =
    { confirming = Nothing, busy = Dict.fromList [ ( 1, Publish.Unpublishing ) ] }


library : Library.Model
library =
    case Username.parse "ada" of
        Ok name ->
            Library.signedIn name (Library.init False)
                |> Tuple.first
                |> Library.enter
                |> Tuple.first
                |> Library.update
                    (Happened
                        (Answered
                            (Idb.Rows
                                { label = ""
                                , rows =
                                    [ Encode.object
                                        [ ( "id", Encode.int 1 )
                                        , ( "blob", Encode.string "blob:1" )
                                        , ( "prompt", Encode.string "a red fox in the snow" )
                                        , ( "width", Encode.int 512 )
                                        , ( "height", Encode.int 512 )
                                        , ( "steps", Encode.int 4 )
                                        , ( "seed", Encode.int 7 )
                                        , ( "model", Encode.string "prism-ml/bonsai-image-ternary-4B-mlx-2bit" )
                                        , ( "owner", Encode.string "ada" )
                                        ]
                                    ]
                                }
                            )
                        )
                    )
                |> Tuple.first

        Err _ ->
            Library.init False
