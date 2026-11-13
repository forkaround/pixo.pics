module EnrolmentTest exposing (suite)

import Ceremony exposing (Ceremony(..))
import Effect exposing (Effect(..))
import Enrolment
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Enrolment"
        [ test "asks for options once, however often it is requested" <|
            \() ->
                case Enrolment.update Enrolment.requested Waiting of
                    ( Waiting, NoEffect, Enrolment.Pending ) ->
                        Expect.pass

                    _ ->
                        Expect.fail "Expected nothing to happen."
        , test "saves no passkey the browser made unasked" <|
            \() ->
                case Enrolment.update (Enrolment.created attestation) (Resting Nothing) of
                    ( Resting Nothing, NoEffect, Enrolment.Pending ) ->
                        Expect.pass

                    _ ->
                        Expect.fail "Expected nothing to happen."
        , test "saves the passkey the browser made when asked" <|
            \() ->
                case Enrolment.update (Enrolment.created attestation) Waiting of
                    ( Waiting, Request request, Enrolment.Pending ) ->
                        ( request.method, request.url )
                            |> Expect.equal ( "POST", "/api/auth/passkeys" )

                    _ ->
                        Expect.fail "Expected a request."
        , test "words why the browser made none" <|
            \() ->
                case Enrolment.update (Enrolment.failed "InvalidStateError") Waiting of
                    ( Resting (Just reason), NoEffect, Enrolment.Pending ) ->
                        reason
                            |> Expect.equal "This device already has a passkey for this account."

                    _ ->
                        Expect.fail "Expected a reason."
        ]


attestation : { clientDataJSON : String, attestationObject : String, transports : List String }
attestation =
    { clientDataJSON = "eyJ0eXBlIjo", attestationObject = "o2NmbXRkbm9uZQ", transports = [ "internal" ] }
