module Path exposing (Path(..), parse, toString)

import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser)


type Path
    = Home
    | SignUp
    | SignIn
    | Verify (Maybe String)
    | Dashboard
    | NotFound


toString : Path -> String
toString path =
    case path of
        Home ->
            "/"

        SignUp ->
            "/auth/signup"

        SignIn ->
            "/auth/signin"

        Verify Nothing ->
            "/auth/verify"

        Verify (Just token) ->
            "/auth/verify#" ++ token

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
        [ Parser.map Home Parser.top
        , Parser.map SignUp (Parser.s "auth" </> Parser.s "signup")
        , Parser.map SignIn (Parser.s "auth" </> Parser.s "signin")
        , Parser.map Verify (Parser.s "auth" </> Parser.s "verify" </> Parser.fragment nonEmpty)
        , Parser.map Dashboard (Parser.s "dashboard")
        ]


nonEmpty : Maybe String -> Maybe String
nonEmpty fragment =
    case fragment of
        Just "" ->
            Nothing

        _ ->
            fragment
