module SignUp exposing (SignUp, parse)

import Api
import EmailAddress exposing (EmailAddress)
import Username exposing (Username)


type alias SignUp =
    { email : EmailAddress
    , username : Username
    }


parse : Api.SignUp -> Result String SignUp
parse { email, username } =
    Result.map2 SignUp
        (EmailAddress.parse email)
        (Username.parse username)
