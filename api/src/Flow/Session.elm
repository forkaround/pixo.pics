module Flow.Session exposing (Msg, current, end, update)

import Api
import Db
import Effect exposing (Effect(..), query, respond, transact)
import InteropDefinitions as Interop
import Reply exposing (Reply(..), withSession)
import Request exposing (Request)
import Response
import Server exposing (Server)
import SessionCookie


type Msg
    = Found Interop.Resolver (Maybe Api.Account)
    | Ended Interop.Resolver


current : Server -> Request -> Reply Msg
current server request =
    withSession
        request
        (\token resolver ->
            transact server.db resolver "Session lookup failed" (Db.account token) (Found resolver)
        )


end : Server -> Request -> Reply Msg
end server request =
    case SessionCookie.read request of
        Just token ->
            Later
                (\resolver ->
                    query server.db resolver "Sign-out failed" (Db.signOut token) (\() -> Ended resolver)
                )

        Nothing ->
            Now (signedOut server)


update : Server -> Msg -> Effect Msg
update server msg =
    case msg of
        Found resolver (Just account) ->
            respond resolver (Response.json 200 (Api.encodeResponse Api.session account))

        Found resolver Nothing ->
            respond resolver Response.notSignedIn

        Ended resolver ->
            respond resolver (signedOut server)


signedOut : Server -> Response.Response
signedOut server =
    Response.json 200 (Api.encodeResponse Api.signOut ())
        |> SessionCookie.clear server.origin
