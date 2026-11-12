module Path exposing (Path(..), parse, toString)

import Data.LinkToken as LinkToken exposing (LinkToken)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser)


type Path
    = Explore
    | Studio
    | Library
    | Models
    | Account
    | SignUp
    | SignIn
    | Verify (Maybe LinkToken)
    | Passkey
    | NotFound


toString : Path -> String
toString path =
    case path of
        Explore ->
            "/"

        Studio ->
            "/studio"

        Library ->
            "/library"

        Models ->
            "/settings/models"

        Account ->
            "/settings/account"

        SignUp ->
            "/signup"

        SignIn ->
            "/login"

        Verify Nothing ->
            "/verify"

        Verify (Just token) ->
            "/verify#" ++ LinkToken.toString token

        Passkey ->
            "/passkey"

        NotFound ->
            "/404"


parse : Url -> Path
parse url =
    Parser.parse parser url
        |> Maybe.withDefault NotFound


parser : Parser (Path -> a) a
parser =
    Parser.oneOf
        [ Parser.map Explore Parser.top
        , Parser.map Studio (Parser.s "studio")
        , Parser.map Library (Parser.s "library")
        , Parser.map Models (Parser.s "settings")
        , Parser.map Models (Parser.s "settings" </> Parser.s "models")
        , Parser.map Account (Parser.s "settings" </> Parser.s "account")
        , Parser.map SignUp (Parser.s "signup")
        , Parser.map SignIn (Parser.s "login")
        , Parser.map Verify (Parser.s "verify" </> Parser.fragment (Maybe.andThen LinkToken.fromString))
        , Parser.map Passkey (Parser.s "passkey")
        ]
