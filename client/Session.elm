module Session exposing (Session(..))

import Api


type Session
    = Unknown
    | Unavailable String
    | SignedOut
    | SignedIn Api.Account
