module Path exposing (Path(..), SyncReturn, parse, toString)

import Data.LinkToken as LinkToken exposing (LinkToken)
import Data.Username as Username exposing (Username)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), (<?>), Parser)
import Url.Parser.Query as Query


type Path
    = Explore
    | Studio
    | Library
    | General
    | Models
    | Account
    | Sync SyncReturn
    | SignUp
    | SignIn
    | Verify (Maybe LinkToken)
    | Passkey
    | Profile Username
    | NotFound


type alias SyncReturn =
    { code : Maybe String
    , state : Maybe String
    , error : Maybe String
    }


toString : Path -> String
toString path =
    case path of
        Explore ->
            "/"

        Studio ->
            "/studio"

        Library ->
            "/library"

        General ->
            "/settings/general"

        Models ->
            "/settings/models"

        Account ->
            "/settings/account"

        Sync _ ->
            "/settings/sync"

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

        Profile name ->
            "/" ++ Username.toString name

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
        , Parser.map General (Parser.s "settings")
        , Parser.map General (Parser.s "settings" </> Parser.s "general")
        , Parser.map Models (Parser.s "settings" </> Parser.s "models")
        , Parser.map Account (Parser.s "settings" </> Parser.s "account")
        , Parser.map (\code state error -> Sync { code = code, state = state, error = error })
            (Parser.s "settings" </> Parser.s "sync" <?> Query.string "code" <?> Query.string "state" <?> Query.string "error")
        , Parser.map SignUp (Parser.s "signup")
        , Parser.map SignIn (Parser.s "login")
        , Parser.map Verify (Parser.s "verify" </> Parser.fragment (Maybe.andThen LinkToken.fromString))
        , Parser.map Passkey (Parser.s "passkey")
        , Parser.map Profile (Parser.custom "USERNAME" (Username.claim >> Result.toMaybe))
        ]
