module Path exposing (Path(..), parse, toString)

import Data.LinkToken as LinkToken exposing (LinkToken)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser)


type Path
    = Home
    | SignUp
    | SignIn
    | Verify (Maybe LinkToken)
    | Dashboard
    | Generate
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
            "/auth/verify#" ++ LinkToken.toString token

        Dashboard ->
            "/dashboard"

        Generate ->
            "/generate"

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
        , Parser.map Verify (Parser.s "auth" </> Parser.s "verify" </> Parser.fragment (Maybe.andThen LinkToken.fromString))
        , Parser.map Dashboard (Parser.s "dashboard")
        , Parser.map Generate (Parser.s "generate")
        ]
