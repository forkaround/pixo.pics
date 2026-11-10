module Path exposing (Path(..), parse, toString)

import Url exposing (Url)


type Path
    = Home
    | SignUp
    | SignIn
    | Verify (Maybe String)
    | Dashboard


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


parse : Url -> Maybe Path
parse url =
    case url.path of
        "/" ->
            Just Home

        "/auth/signup" ->
            Just SignUp

        "/auth/signin" ->
            Just SignIn

        "/auth/verify" ->
            Just
                (Verify
                    (case url.fragment of
                        Just "" ->
                            Nothing

                        fragment ->
                            fragment
                    )
                )

        "/dashboard" ->
            Just Dashboard

        _ ->
            Nothing
