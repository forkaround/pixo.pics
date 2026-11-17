module Flow.Session exposing (Msg, current, deleteAccount, end, update)

import Api
import Data.PictureId as PictureId exposing (PictureId)
import Data.SessionToken exposing (SessionToken)
import Db
import Effect exposing (Effect(..), log, query, respond, transact)
import InteropDefinitions as Interop
import Reply exposing (Reply(..), withSession)
import Request exposing (Request)
import Response exposing (Response)
import Server exposing (Server)
import Services.Paddle as Paddle
import Services.R2 as R2
import SessionCookie
import Time


type Msg
    = Found Interop.Resolver (Maybe ( Api.Account, List Db.Subscription ))
    | Ended Interop.Resolver
    | Deleted Interop.Resolver Db.Deletion
    | BeforeDeletion Interop.Resolver SessionToken (Maybe { renewing : Bool, pictures : List PictureId })
    | FilesTimed Interop.Resolver SessionToken (List PictureId) Int Time.Posix
    | FileDeleted Interop.Resolver SessionToken PictureId (List PictureId) Int (Result String ())
    | FileDropped Interop.Resolver SessionToken (List PictureId) Int


maxFilesPerRequest : Int
maxFilesPerRequest =
    300


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
            transact server.db resolver "Account deletion failed" (Db.beforeDeletion token) (BeforeDeletion resolver token)
        )


deletion : Server -> Interop.Resolver -> SessionToken -> Effect Msg
deletion server resolver token =
    transact server.db resolver "Account deletion failed" (Db.deleteAccount token) (Deleted resolver)


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

        BeforeDeletion resolver _ Nothing ->
            respond resolver Response.notSignedIn

        BeforeDeletion resolver token (Just before) ->
            if before.renewing then
                respond resolver (Response.error 409 "Cancel Pro before deleting your account.")

            else if List.isEmpty before.pictures then
                deletion server resolver token

            else
                Clock (FilesTimed resolver token before.pictures 0)

        FilesTimed resolver token remaining count time ->
            case ( remaining, server.storage ) of
                ( [], _ ) ->
                    deletion server resolver token

                ( _ :: _, Nothing ) ->
                    Batch
                        [ respond resolver Response.unavailable
                        , log Interop.Error "Can't delete an account with published pictures without R2 settings."
                        ]

                ( id :: rest, Just storage ) ->
                    if count >= maxFilesPerRequest then
                        respond resolver (Response.error 503 "Still deleting your pictures. Try again.")

                    else
                        Effect.map (FileDeleted resolver token id rest count) (R2.delete storage time id)

        FileDeleted resolver token id rest count (Ok ()) ->
            transact server.db
                resolver
                "Picture removal failed"
                (Db.dropPicture token id)
                (\_ -> FileDropped resolver token rest (count + 1))

        FileDeleted resolver _ id _ _ (Err reason) ->
            Batch
                [ respond resolver (Response.error 503 "Couldn't delete your pictures. Try again.")
                , log Interop.Error ("R2 delete failed for " ++ PictureId.toString id ++ ": " ++ reason)
                ]

        FileDropped resolver token rest count ->
            Clock (FilesTimed resolver token rest count)

        Ended resolver ->
            respond resolver (signedOut server)


signedOut : Server -> Response
signedOut server =
    Response.json 200 (Api.encodeResponse Api.signOut ())
        |> SessionCookie.clear server.origin
