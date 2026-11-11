module Api exposing
    ( Account
    , Assertion
    , Attestation
    , Check
    , Endpoint
    , Health
    , Method(..)
    , Passkey
    , PasskeyOptions
    , Problem(..)
    , Release
    , Route(..)
    , SignIn
    , SignInOptions
    , SignUp
    , Status(..)
    , Verification
    , addPasskey
    , bodyDecoder
    , checkNames
    , checks
    , deletePasskey
    , encodeBody
    , encodeError
    , encodeResponse
    , errorDecoder
    , health
    , healthOf
    , method
    , methodToString
    , parse
    , passkeyOptions
    , passkeySignIn
    , passkeySignInOptions
    , passkeys
    , path
    , responseDecoder
    , route
    , session
    , signIn
    , signOut
    , signUp
    , toString
    , verification
    , verify
    )

import Data.Email as Email exposing (Email)
import Data.Username as Username exposing (Username)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Url



-- ROUTES


type Route
    = GetHealth
    | PostSignUp
    | PostSignIn
    | GetSession
    | DeleteSession
    | GetVerification
    | PostVerification
    | GetPasskeys
    | PostPasskey
    | PostPasskeyOptions
    | DeletePasskey String
    | PostPasskeySignIn
    | PostPasskeySignInOptions


type Method
    = Get
    | Post
    | Delete


type Problem
    = NotFound
    | MethodNotAllowed (List Method)


method : Route -> Method
method r =
    case r of
        GetHealth ->
            Get

        PostSignUp ->
            Post

        PostSignIn ->
            Post

        GetSession ->
            Get

        DeleteSession ->
            Delete

        GetVerification ->
            Get

        PostVerification ->
            Post

        GetPasskeys ->
            Get

        PostPasskey ->
            Post

        PostPasskeyOptions ->
            Post

        DeletePasskey _ ->
            Delete

        PostPasskeySignIn ->
            Post

        PostPasskeySignInOptions ->
            Post


path : Route -> List String
path r =
    "api" :: segments r


segments : Route -> List String
segments r =
    case r of
        GetHealth ->
            [ "health" ]

        PostSignUp ->
            [ "auth", "signup" ]

        PostSignIn ->
            [ "auth", "signin" ]

        GetSession ->
            [ "auth", "session" ]

        DeleteSession ->
            [ "auth", "session" ]

        GetVerification ->
            [ "auth", "verify" ]

        PostVerification ->
            [ "auth", "verify" ]

        GetPasskeys ->
            [ "auth", "passkeys" ]

        PostPasskey ->
            [ "auth", "passkeys" ]

        PostPasskeyOptions ->
            [ "auth", "passkeys", "options" ]

        DeletePasskey id ->
            [ "auth", "passkeys", id ]

        PostPasskeySignIn ->
            [ "auth", "signin", "passkey" ]

        PostPasskeySignInOptions ->
            [ "auth", "signin", "passkey", "options" ]


toString : Route -> String
toString r =
    "/" ++ String.join "/" (List.map Url.percentEncode (path r))


methodToString : Method -> String
methodToString m =
    case m of
        Get ->
            "GET"

        Post ->
            "POST"

        Delete ->
            "DELETE"


parse : String -> List String -> Result Problem Route
parse methodName pathSegments =
    let
        candidates =
            case pathSegments of
                "api" :: rest ->
                    routesAt rest

                _ ->
                    []
    in
    case List.filter (\r -> methodToString (method r) == methodName) candidates of
        r :: _ ->
            Ok r

        [] ->
            if List.isEmpty candidates then
                Err NotFound

            else
                Err (MethodNotAllowed (List.map method candidates))


routesAt : List String -> List Route
routesAt rest =
    case rest of
        [ "health" ] ->
            [ GetHealth ]

        [ "auth", "signup" ] ->
            [ PostSignUp ]

        [ "auth", "signin" ] ->
            [ PostSignIn ]

        [ "auth", "session" ] ->
            [ GetSession, DeleteSession ]

        [ "auth", "verify" ] ->
            [ GetVerification, PostVerification ]

        [ "auth", "passkeys" ] ->
            [ GetPasskeys, PostPasskey ]

        [ "auth", "passkeys", "options" ] ->
            [ PostPasskeyOptions, DeletePasskey "options" ]

        [ "auth", "passkeys", id ] ->
            [ DeletePasskey id ]

        [ "auth", "signin", "passkey" ] ->
            [ PostPasskeySignIn ]

        [ "auth", "signin", "passkey", "options" ] ->
            [ PostPasskeySignInOptions ]

        _ ->
            []



-- ENDPOINTS


type Endpoint body response
    = Endpoint
        { route : Route
        , body : Body body
        , response : Codec response
        }


type Body a
    = NoBody a
    | JsonBody (Codec a)


type alias Codec a =
    { encode : a -> Encode.Value
    , decoder : Decoder a
    }


health : Endpoint () Health
health =
    Endpoint { route = GetHealth, body = NoBody (), response = healthCodec }


signUp : Endpoint SignUp ()
signUp =
    Endpoint { route = PostSignUp, body = JsonBody signUpCodec, response = emptyCodec }


signIn : Endpoint SignIn ()
signIn =
    Endpoint { route = PostSignIn, body = JsonBody signInCodec, response = emptyCodec }


session : Endpoint () Account
session =
    Endpoint { route = GetSession, body = NoBody (), response = accountCodec }


signOut : Endpoint () ()
signOut =
    Endpoint { route = DeleteSession, body = NoBody (), response = emptyCodec }


verification : Endpoint () Verification
verification =
    Endpoint { route = GetVerification, body = NoBody (), response = verificationCodec }


verify : Endpoint () Account
verify =
    Endpoint { route = PostVerification, body = NoBody (), response = accountCodec }


passkeys : Endpoint () (List Passkey)
passkeys =
    Endpoint { route = GetPasskeys, body = NoBody (), response = listCodec passkeyCodec }


passkeyOptions : Endpoint () PasskeyOptions
passkeyOptions =
    Endpoint { route = PostPasskeyOptions, body = NoBody (), response = passkeyOptionsCodec }


addPasskey : Endpoint Attestation Passkey
addPasskey =
    Endpoint { route = PostPasskey, body = JsonBody attestationCodec, response = passkeyCodec }


deletePasskey : String -> Endpoint () ()
deletePasskey id =
    Endpoint { route = DeletePasskey id, body = NoBody (), response = emptyCodec }


passkeySignInOptions : Endpoint () SignInOptions
passkeySignInOptions =
    Endpoint { route = PostPasskeySignInOptions, body = NoBody (), response = signInOptionsCodec }


passkeySignIn : Endpoint Assertion Account
passkeySignIn =
    Endpoint { route = PostPasskeySignIn, body = JsonBody assertionCodec, response = accountCodec }


route : Endpoint body response -> Route
route (Endpoint e) =
    e.route


encodeBody : Endpoint body response -> body -> Maybe Encode.Value
encodeBody (Endpoint e) value =
    case e.body of
        NoBody _ ->
            Nothing

        JsonBody codec ->
            Just (codec.encode value)


bodyDecoder : Endpoint body response -> Decoder body
bodyDecoder (Endpoint e) =
    case e.body of
        NoBody value ->
            Decode.succeed value

        JsonBody codec ->
            codec.decoder


encodeResponse : Endpoint body response -> response -> Encode.Value
encodeResponse (Endpoint e) =
    e.response.encode


responseDecoder : Endpoint body response -> Decoder response
responseDecoder (Endpoint e) =
    e.response.decoder


encodeError : String -> Encode.Value
encodeError message =
    Encode.object [ ( "error", Encode.string message ) ]


errorDecoder : Decoder String
errorDecoder =
    Decode.field "error" Decode.string



-- PAYLOADS


type alias SignUp =
    { email : String
    , username : String
    }


signUpCodec : Codec SignUp
signUpCodec =
    { encode =
        \s ->
            Encode.object
                [ ( "email", Encode.string s.email )
                , ( "username", Encode.string s.username )
                ]
    , decoder =
        Decode.map2 SignUp
            (Decode.field "email" Decode.string)
            (Decode.field "username" Decode.string)
    }


type alias SignIn =
    { email : String
    }


signInCodec : Codec SignIn
signInCodec =
    { encode = \s -> Encode.object [ ( "email", Encode.string s.email ) ]
    , decoder = Decode.map SignIn (Decode.field "email" Decode.string)
    }


type alias Verification =
    { email : Email
    , username : Username
    , newAccount : Bool
    }


verificationCodec : Codec Verification
verificationCodec =
    { encode =
        \v ->
            Encode.object
                [ ( "email", Email.encode v.email )
                , ( "username", Username.encode v.username )
                , ( "newAccount", Encode.bool v.newAccount )
                ]
    , decoder =
        Decode.map3 Verification
            (Decode.field "email" Email.decoder)
            (Decode.field "username" Username.decoder)
            (Decode.field "newAccount" Decode.bool)
    }


type alias Account =
    { email : Email
    , username : Username
    }


accountCodec : Codec Account
accountCodec =
    { encode =
        \a ->
            Encode.object
                [ ( "email", Email.encode a.email )
                , ( "username", Username.encode a.username )
                ]
    , decoder =
        Decode.map2 Account
            (Decode.field "email" Email.decoder)
            (Decode.field "username" Username.decoder)
    }


type alias Passkey =
    { id : String
    , name : Maybe String
    , createdAt : String
    }


passkeyCodec : Codec Passkey
passkeyCodec =
    { encode =
        \p ->
            Encode.object
                [ ( "id", Encode.string p.id )
                , ( "name", Maybe.withDefault Encode.null (Maybe.map Encode.string p.name) )
                , ( "createdAt", Encode.string p.createdAt )
                ]
    , decoder =
        Decode.map3 Passkey
            (Decode.field "id" Decode.string)
            (Decode.field "name" (Decode.nullable Decode.string))
            (Decode.field "createdAt" Decode.string)
    }


type alias PasskeyOptions =
    { challenge : String
    , rp : { id : String, name : String }
    , user : { id : String, name : String, displayName : String }
    , algorithms : List Int
    , excludeCredentials : List String
    }


passkeyOptionsCodec : Codec PasskeyOptions
passkeyOptionsCodec =
    { encode =
        \o ->
            Encode.object
                [ ( "challenge", Encode.string o.challenge )
                , ( "rp"
                  , Encode.object
                        [ ( "id", Encode.string o.rp.id )
                        , ( "name", Encode.string o.rp.name )
                        ]
                  )
                , ( "user"
                  , Encode.object
                        [ ( "id", Encode.string o.user.id )
                        , ( "name", Encode.string o.user.name )
                        , ( "displayName", Encode.string o.user.displayName )
                        ]
                  )
                , ( "algorithms", Encode.list Encode.int o.algorithms )
                , ( "excludeCredentials", Encode.list Encode.string o.excludeCredentials )
                ]
    , decoder =
        Decode.map5 PasskeyOptions
            (Decode.field "challenge" Decode.string)
            (Decode.field "rp"
                (Decode.map2 (\id name -> { id = id, name = name })
                    (Decode.field "id" Decode.string)
                    (Decode.field "name" Decode.string)
                )
            )
            (Decode.field "user"
                (Decode.map3 (\id name displayName -> { id = id, name = name, displayName = displayName })
                    (Decode.field "id" Decode.string)
                    (Decode.field "name" Decode.string)
                    (Decode.field "displayName" Decode.string)
                )
            )
            (Decode.field "algorithms" (Decode.list Decode.int))
            (Decode.field "excludeCredentials" (Decode.list Decode.string))
    }


type alias Attestation =
    { clientDataJSON : String
    , attestationObject : String
    , transports : List String
    }


attestationCodec : Codec Attestation
attestationCodec =
    { encode =
        \a ->
            Encode.object
                [ ( "clientDataJSON", Encode.string a.clientDataJSON )
                , ( "attestationObject", Encode.string a.attestationObject )
                , ( "transports", Encode.list Encode.string a.transports )
                ]
    , decoder =
        Decode.map3 Attestation
            (Decode.field "clientDataJSON" Decode.string)
            (Decode.field "attestationObject" Decode.string)
            (Decode.field "transports" (Decode.list Decode.string))
    }


type alias SignInOptions =
    { challenge : String
    , rpId : String
    }


signInOptionsCodec : Codec SignInOptions
signInOptionsCodec =
    { encode =
        \o ->
            Encode.object
                [ ( "challenge", Encode.string o.challenge )
                , ( "rpId", Encode.string o.rpId )
                ]
    , decoder =
        Decode.map2 SignInOptions
            (Decode.field "challenge" Decode.string)
            (Decode.field "rpId" Decode.string)
    }


type alias Assertion =
    { id : String
    , clientDataJSON : String
    , authenticatorData : String
    , signature : String
    }


assertionCodec : Codec Assertion
assertionCodec =
    { encode =
        \a ->
            Encode.object
                [ ( "id", Encode.string a.id )
                , ( "clientDataJSON", Encode.string a.clientDataJSON )
                , ( "authenticatorData", Encode.string a.authenticatorData )
                , ( "signature", Encode.string a.signature )
                ]
    , decoder =
        Decode.map4 Assertion
            (Decode.field "id" Decode.string)
            (Decode.field "clientDataJSON" Decode.string)
            (Decode.field "authenticatorData" Decode.string)
            (Decode.field "signature" Decode.string)
    }


listCodec : Codec a -> Codec (List a)
listCodec codec =
    { encode = Encode.list codec.encode
    , decoder = Decode.list codec.decoder
    }


emptyCodec : Codec ()
emptyCodec =
    { encode = \() -> Encode.object []
    , decoder = Decode.succeed ()
    }


type Status
    = Pass
    | Warn
    | Fail


type alias Check =
    { status : Status
    , latencyMs : Maybe Int
    , output : Maybe String
    }


type alias Release =
    { releaseId : Maybe String
    , releaseUrl : Maybe String
    , region : Maybe String
    }


type alias Health =
    { status : Status
    , release : Release
    , database : Check
    }


healthOf : Release -> Check -> Health
healthOf release database =
    { status = database.status, release = release, database = database }


checks : Health -> List ( String, Check )
checks health_ =
    List.map (\( name, check ) -> ( name, check health_ )) namedChecks


checkNames : List String
checkNames =
    List.map Tuple.first namedChecks


namedChecks : List ( String, Health -> Check )
namedChecks =
    [ ( "Database", .database ) ]


healthCodec : Codec Health
healthCodec =
    { encode =
        \h ->
            Encode.object
                (List.filterMap identity
                    [ Just ( "status", encodeStatus h.status )
                    , Maybe.map (\id -> ( "releaseId", Encode.string id )) h.release.releaseId
                    , Maybe.map (\region -> ( "region", Encode.string region )) h.release.region
                    , Maybe.map (\url -> ( "links", Encode.object [ ( "release", Encode.string url ) ] )) h.release.releaseUrl
                    , Just ( "checks", Encode.object [ ( "database", encodeCheck h.database ) ] )
                    ]
                )
    , decoder =
        Decode.map3 Health
            (Decode.field "status" statusDecoder)
            (Decode.map3 Release
                (Decode.maybe (Decode.field "releaseId" Decode.string))
                (Decode.maybe (Decode.at [ "links", "release" ] Decode.string))
                (Decode.maybe (Decode.field "region" Decode.string))
            )
            (Decode.at [ "checks", "database" ] checkDecoder)
    }


encodeCheck : Check -> Encode.Value
encodeCheck check =
    Encode.object
        (List.filterMap identity
            [ Just ( "status", encodeStatus check.status )
            , Maybe.map (\ms -> ( "latencyMs", Encode.int ms )) check.latencyMs
            , Maybe.map (\output -> ( "output", Encode.string output )) check.output
            ]
        )


checkDecoder : Decoder Check
checkDecoder =
    Decode.map3 Check
        (Decode.field "status" statusDecoder)
        (Decode.maybe (Decode.field "latencyMs" Decode.int))
        (Decode.maybe (Decode.field "output" Decode.string))


encodeStatus : Status -> Encode.Value
encodeStatus status =
    case status of
        Pass ->
            Encode.string "pass"

        Warn ->
            Encode.string "warn"

        Fail ->
            Encode.string "fail"


statusDecoder : Decoder Status
statusDecoder =
    Decode.string
        |> Decode.andThen
            (\status ->
                case status of
                    "pass" ->
                        Decode.succeed Pass

                    "warn" ->
                        Decode.succeed Warn

                    "fail" ->
                        Decode.succeed Fail

                    _ ->
                        Decode.fail ("Unknown health status: " ++ status)
            )
