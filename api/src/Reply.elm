module Reply exposing (Reply(..), map, withSession)

import Data.SessionToken exposing (SessionToken)
import Effect exposing (Effect)
import InteropDefinitions as Interop
import RateLimit
import Request exposing (Request)
import Response exposing (Response)
import SessionCookie


type Reply msg
    = Now Response
    | Later (Interop.Resolver -> Effect msg)
    | Limit (List RateLimit.Policy) (Reply msg)


map : (a -> b) -> Reply a -> Reply b
map toMsg reply =
    case reply of
        Now response ->
            Now response

        Later toEffect ->
            Later (toEffect >> Effect.map toMsg)

        Limit policies next ->
            Limit policies (map toMsg next)


withSession : Request -> (SessionToken -> Interop.Resolver -> Effect msg) -> Reply msg
withSession request toEffect =
    case SessionCookie.read request of
        Just token ->
            Later (toEffect token)

        Nothing ->
            Now Response.notSignedIn
