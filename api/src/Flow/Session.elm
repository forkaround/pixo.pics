module Flow.Session exposing (Msg, current, deleteAccount, end, update)

import Api
import Db
import Effect exposing (Effect(..), query, respond, transact)
import InteropDefinitions as Interop
import Reply exposing (Reply(..), withSession)
import Request exposing (Request)
import Response exposing (Response)
import Server exposing (Server)
import Services.Paddle as Paddle
import SessionCookie


type Msg
    = Found Interop.Resolver (Maybe ( Api.Account, List Db.Subscription ))
    | Ended Interop.Resolver
    | Deleted Interop.Resolver Db.Deletion


current : Server -> Request -> Reply Msg
current server request =
    withSession
        request
        (\token resolver ->
            transact server.db resolver "Session lookup failed" (Db.me token) (Found resolver)
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


deleteAccount : Server -> Request -> Reply Msg
deleteAccount server request =
    withSession request
        (\token resolver ->
            transact server.db resolver "Account deletion failed" (Db.deleteAccount token) (Deleted resolver)
        )


update : Server -> Msg -> Effect Msg
update server msg =
    case msg of
        Found resolver (Just ( account, rows )) ->
            respond resolver
                (Response.json 200
                    (Api.encodeResponse Api.session { account = account, billing = Paddle.billingOf rows })
                )

        Found resolver Nothing ->
            respond resolver Response.notSignedIn

        Deleted resolver Db.Deleted ->
            respond resolver
                (Response.json 200 (Api.encodeResponse Api.deleteAccount ())
                    |> SessionCookie.clear server.origin
                )

        Deleted resolver Db.NotSignedIn ->
            respond resolver Response.notSignedIn

        Deleted resolver Db.Renewing ->
            respond resolver (Response.error 409 "Cancel Pro before deleting your account.")

        Ended resolver ->
            respond resolver (signedOut server)


signedOut : Server -> Response
signedOut server =
    Response.json 200 (Api.encodeResponse Api.signOut ())
        |> SessionCookie.clear server.origin
