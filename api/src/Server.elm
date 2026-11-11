module Server exposing (Server)

import Api
import Data.Origin exposing (Origin)
import Services.Mailer as Mailer
import Services.Neon as Neon


type alias Server =
    { db : Neon.Connection
    , mailer : Result String Mailer.Mailer
    , origin : Origin
    , release : Api.Release
    }
