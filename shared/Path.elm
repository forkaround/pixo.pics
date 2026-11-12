module Path exposing (Path(..), parse, toString)

import Data.LinkToken as LinkToken exposing (LinkToken)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser)


type Path
    = Explore
    | Studio
    | Library
    | Models
    | Server
    | SignUp
    | SignIn
    | Verify (Maybe LinkToken)
    | Dashboard
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

        Server ->
            "/settings/server"

        SignUp ->
            "/auth/signup"

        SignIn ->
            "/auth/signin"

        Verify Nothing ->
            "/auth/verify"

        Verify (Just token) ->
            "/auth/verify#" ++ LinkToken.toString token

        Dashboard ->
            "/dashboard"

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
        , Parser.map Server (Parser.s "settings" </> Parser.s "server")
        , Parser.map SignUp (Parser.s "auth" </> Parser.s "signup")
        , Parser.map SignIn (Parser.s "auth" </> Parser.s "signin")
        , Parser.map Verify (Parser.s "auth" </> Parser.s "verify" </> Parser.fragment (Maybe.andThen LinkToken.fromString))
        , Parser.map Dashboard (Parser.s "dashboard")
        ]
