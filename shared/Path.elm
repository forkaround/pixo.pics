module Path exposing (Path(..), parse, toString)

import Url exposing (Url)


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
    case url.path of
        "/" ->
            Home

        "/auth/signup" ->
            SignUp

        "/auth/signin" ->
            SignIn

        "/auth/verify" ->
            Verify
                (case url.fragment of
                    Just "" ->
                        Nothing

                    fragment ->
                        fragment
                )

        "/dashboard" ->
            Dashboard

        _ ->
            NotFound
