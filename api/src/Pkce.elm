module Pkce exposing (challenge)

import SHA256


challenge : String -> String
challenge verifier =
    SHA256.toBase64 (SHA256.fromString verifier)
        |> String.map
            (\c ->
                case c of
                    '+' ->
                        '-'

                    '/' ->
                        '_'

                    _ ->
                        c
            )
        |> String.filter (\c -> c /= '=')
