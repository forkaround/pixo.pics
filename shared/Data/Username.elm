module Data.Username exposing (Username, claim, decoder, encode, parse, toString)

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type Username
    = Username String


parse : String -> Result String Username
parse value =
    let
        clean =
            String.toLower (String.trim value)
    in
    if isUsername clean then
        Ok (Username clean)

    else
        Err "Not a valid username."


claim : String -> Result String Username
claim value =
    parse value
        |> Result.andThen
            (\username ->
                if List.member (toString username) reserved then
                    Err "That username is reserved."

                else
                    Ok username
            )


reserved : List String
reserved =
    [ "404"
    , "about"
    , "account"
    , "accounts"
    , "admin"
    , "api"
    , "app"
    , "assets"
    , "billing"
    , "blog"
    , "client"
    , "contact"
    , "docs"
    , "explore"
    , "feed"
    , "help"
    , "home"
    , "img"
    , "images"
    , "legal"
    , "library"
    , "login"
    , "logout"
    , "me"
    , "moderation"
    , "new"
    , "passkey"
    , "pictures"
    , "pixo"
    , "pricing"
    , "privacy"
    , "pro"
    , "profile"
    , "profiles"
    , "refunds"
    , "register"
    , "report"
    , "root"
    , "search"
    , "security"
    , "settings"
    , "signin"
    , "signup"
    , "static"
    , "status"
    , "studio"
    , "support"
    , "sync"
    , "tags"
    , "terms"
    , "user"
    , "users"
    , "verify"
    , "www"
    ]


decoder : Decoder Username
decoder =
    Decode.string
        |> Decode.andThen
            (\value ->
                case parse value of
                    Ok parsed ->
                        Decode.succeed parsed

                    Err problem ->
                        Decode.fail problem
            )


encode : Username -> Encode.Value
encode (Username value) =
    Encode.string value


toString : Username -> String
toString (Username value) =
    value


isUsername : String -> Bool
isUsername username =
    (String.length username >= 3)
        && (String.length username <= 20)
        && String.all (\char -> Char.isLower char || Char.isDigit char || char == '_') username
