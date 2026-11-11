module Data.SignUp exposing (SignUp, parse)

import Api
import Data.Email as Email exposing (Email)
import Data.Username as Username exposing (Username)


type alias SignUp =
    { email : Email
    , username : Username
    }


parse : Api.SignUp -> Result String SignUp
parse { email, username } =
    Result.map2 SignUp
        (Email.parse email)
        (Username.parse username)
