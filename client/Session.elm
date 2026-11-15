module Session exposing (Session(..))

import Api


type Session
    = Unknown
    | SignedOut
    | SignedIn Api.Account
