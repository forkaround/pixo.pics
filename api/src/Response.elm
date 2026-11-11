module Response exposing (Response, accepted, error, health, invalidLink, json, notRouted, notSignedIn, tooManyRequests, unavailable, withHeader)

import Api
import Json.Encode as Encode


type alias Response =
    { status : Int
    , headers : List ( String, String )
    , body : String
    }


withHeader : String -> String -> Response -> Response
withHeader name value response =
    { response | headers = ( name, value ) :: response.headers }


json : Int -> Encode.Value -> Response
json status value =
    { status = status
    , headers = [ ( "content-type", "application/json" ), ( "cache-control", "no-store" ) ]
    , body = Encode.encode 0 value
    }


health : Api.Release -> Api.Check -> Response
health release check =
    let
        report =
            Api.healthOf release check
    in
    { status =
        case report.status of
            Api.Pass ->
                200

            Api.Warn ->
                200

            Api.Fail ->
                503
    , headers = [ ( "content-type", "application/health+json" ), ( "cache-control", "no-store" ) ]
    , body = Encode.encode 0 (Api.encodeResponse Api.health report)
    }


unavailable : Response
unavailable =
    error 503 "Not available right now. Try again later."


accepted : Response
accepted =
    json 202 (Api.encodeResponse Api.signUp ())


notSignedIn : Response
notSignedIn =
    error 401 "Not signed in."


invalidLink : Response
invalidLink =
    error 404 "This link is not valid any more. Sign up again to get a new one."


tooManyRequests : Int -> Response
tooManyRequests seconds =
    error 429 "Too many attempts. Try again later."
        |> withHeader "retry-after" (String.fromInt seconds)


error : Int -> String -> Response
error status message =
    json status (Api.encodeError message)


notRouted : Api.Problem -> Response
notRouted refused =
    case refused of
        Api.NotFound ->
            error 404 "Not found."

        Api.MethodNotAllowed allowed ->
            error 405 "Method not allowed."
                |> withHeader "allow" (String.join ", " (List.map Api.methodToString allowed))
