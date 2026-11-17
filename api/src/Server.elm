module Server exposing (Server)

import Api
import Data.Origin exposing (Origin)
import Services.Mailer as Mailer
import Services.Neon as Neon
import Services.Paddle as Paddle
import Services.R2 as R2


type alias Server =
    { db : Neon.Connection
    , mailer : Mailer.Mailer
    , origin : Origin
    , paddle : Maybe Paddle.Config
    , release : Api.Release
    , storage : Maybe R2.Config
    }
