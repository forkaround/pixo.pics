module Flow.Pictures exposing (Msg, confirm, publish, report, unpublish, update)

import Api
import Data.Origin as Origin
import Data.PictureId as PictureId exposing (PictureId)
import Data.Publication as Publication
import Data.SessionToken exposing (SessionToken)
import Data.Username exposing (Username)
import Db
import Effect exposing (Effect(..), log, query, respond, transact)
import InteropDefinitions as Interop
import Json.Decode as Decode
import Json.Encode as Encode
import Path
import RateLimit
import Reply exposing (Reply(..), withSession)
import Request exposing (Request)
import Response
import Server exposing (Server)
import Services.Mailer as Mailer
import Services.Mailer.PictureReported
import Services.R2 as R2
import Time


type Msg
    = Asked Interop.Resolver (Maybe Db.Asked)
    | Linked Interop.Resolver PictureId Time.Posix
    | Checked Interop.Resolver SessionToken PictureId (Maybe Db.Unconfirmed)
    | Timed Interop.Resolver SessionToken PictureId Int Time.Posix
    | Headed Interop.Resolver SessionToken PictureId Int Time.Posix (Result String (Maybe R2.Staged))
    | Copied Interop.Resolver SessionToken PictureId Time.Posix (Result String R2.Copied)
    | Cleaned Interop.Resolver SessionToken PictureId (Result String ())
    | Marked Interop.Resolver SessionToken PictureId (Maybe String)
    | Rechecked Interop.Resolver PictureId (Maybe Db.Unconfirmed)
    | PublicRemoved Interop.Resolver SessionToken PictureId Time.Posix Done (Result String ())
    | StagedRemoved Interop.Resolver SessionToken PictureId Done (Result String ())
    | Dropped Interop.Resolver Done Bool
    | Owned Interop.Resolver SessionToken PictureId (Maybe Bool)
    | Unpublishing Interop.Resolver SessionToken PictureId Time.Posix
    | PublicDeleted Interop.Resolver SessionToken PictureId Time.Posix (Result String ())
    | Reported Interop.Resolver PictureId String { outcome : String, first : Bool, username : Maybe Username }
    | Notified Interop.Resolver Mailer.Outcome


type Done
    = Refused
    | Replaced
    | Unpublished


publish : Server -> Request -> Reply Msg
publish server request =
    case server.storage of
        Nothing ->
            Now Response.unavailable

        Just _ ->
            case Decode.decodeString (Api.bodyDecoder Api.publish) request.body of
                Err _ ->
                    Now (Response.error 400 "Expected a JSON object with a prompt, a width, a height, steps, a seed, a model and bytes.")

                Ok raw ->
                    case Publication.parse raw of
                        Err reason ->
                            Now (Response.error 422 reason)

                        Ok publication ->
                            Limit [ RateLimit.publishes (Request.clientAddress request) ]
                                (withSession request
                                    (\token resolver ->
                                        transact server.db resolver "Picture ask failed" (Db.askPicture token publication) (Asked resolver)
                                    )
                                )


confirm : PictureId -> Server -> Request -> Reply Msg
confirm id server request =
    case server.storage of
        Nothing ->
            Now Response.unavailable

        Just _ ->
            withSession request
                (\token resolver ->
                    transact server.db resolver "Picture lookup failed" (Db.unconfirmed token id) (Checked resolver token id)
                )


unpublish : PictureId -> Server -> Request -> Reply Msg
unpublish id server request =
    case server.storage of
        Nothing ->
            Now Response.unavailable

        Just _ ->
            withSession request
                (\token resolver ->
                    transact server.db resolver "Picture lookup failed" (Db.ownPicture token id) (Owned resolver token id)
                )


reasons : List String
reasons =
    [ "illegal", "sexual", "violent", "hateful", "spam", "other" ]


report : PictureId -> Server -> Request -> Reply Msg
report id server request =
    case Decode.decodeString (Api.bodyDecoder (Api.report id)) request.body of
        Err _ ->
            Now (Response.error 400 "Expected a JSON object with a reason.")

        Ok body ->
            if List.member body.reason reasons then
                Limit [ RateLimit.reports (Request.clientAddress request) ]
                    (Later
                        (\resolver ->
                            query server.db resolver "Report failed" (Db.reportPicture id body.reason) (Reported resolver id body.reason)
                        )
                    )

            else
                Now (Response.error 422 "Unknown reason.")


reportUpdate : Server -> Msg -> Maybe (Effect Msg)
reportUpdate server msg =
    case msg of
        Reported resolver id reason found ->
            Just
                (if found.outcome /= "reported" then
                    respond resolver (Response.error 404 "No such picture.")

                 else
                    case ( server.moderators, server.storage, found.username ) of
                        ( Just moderators, Just storage, Just username ) ->
                            if found.first then
                                Mailer.send server.mailer
                                    { to = moderators, tag = "report" }
                                    (Services.Mailer.PictureReported.email
                                        { url = R2.publicUrl storage id
                                        , profile = Origin.href server.origin (Path.Profile username)
                                        , reason = reason
                                        , pictureId = PictureId.toString id
                                        }
                                    )
                                    (Notified resolver)

                            else
                                respond resolver reportAccepted

                        _ ->
                            respond resolver reportAccepted
                )

        Notified resolver (Ok ()) ->
            Just (respond resolver reportAccepted)

        Notified resolver (Err problem) ->
            Just
                (Batch
                    [ respond resolver reportAccepted
                    , log Interop.Error ("Report email failed: " ++ problem)
                    ]
                )

        _ ->
            Nothing


reportAccepted : Response.Response
reportAccepted =
    Response.json 202 (Encode.object [])


resolverOf : Msg -> Interop.Resolver
resolverOf msg =
    case msg of
        Asked resolver _ ->
            resolver

        Linked resolver _ _ ->
            resolver

        Checked resolver _ _ _ ->
            resolver

        Timed resolver _ _ _ _ ->
            resolver

        Headed resolver _ _ _ _ _ ->
            resolver

        Copied resolver _ _ _ _ ->
            resolver

        Cleaned resolver _ _ _ ->
            resolver

        Marked resolver _ _ _ ->
            resolver

        Rechecked resolver _ _ ->
            resolver

        PublicRemoved resolver _ _ _ _ _ ->
            resolver

        StagedRemoved resolver _ _ _ _ ->
            resolver

        Dropped resolver _ _ ->
            resolver

        Owned resolver _ _ _ ->
            resolver

        Unpublishing resolver _ _ _ ->
            resolver

        PublicDeleted resolver _ _ _ _ ->
            resolver

        Reported resolver _ _ _ ->
            resolver

        Notified resolver _ ->
            resolver


update : Server -> Msg -> Effect Msg
update server msg =
    case reportUpdate server msg of
        Just effect ->
            effect

        Nothing ->
            pictureUpdate server msg


pictureUpdate : Server -> Msg -> Effect Msg
pictureUpdate server msg =
    case server.storage of
        Nothing ->
            respond (resolverOf msg) Response.unavailable

        Just storage ->
            case msg of
                Asked resolver Nothing ->
                    respond resolver Response.notSignedIn

                Asked resolver (Just Db.NotPro) ->
                    respond resolver (Response.error 403 "Publishing needs Pro.")

                Asked resolver (Just Db.TooManyUnfinished) ->
                    respond resolver (Response.error 429 "Finish or cancel your other publishes first.")

                Asked resolver (Just (Db.AskedFor id)) ->
                    Clock (Linked resolver id)

                Linked resolver id time ->
                    let
                        link =
                            R2.uploadLink storage time id
                    in
                    respond resolver
                        (Response.json 201
                            (Api.encodeResponse Api.publish { id = id, url = link.url, headers = link.headers })
                        )

                Checked resolver _ _ Nothing ->
                    respond resolver Response.notSignedIn

                Checked resolver _ _ (Just Db.Missing) ->
                    respond resolver (Response.error 404 "No such picture.")

                Checked resolver _ id (Just (Db.AlreadyPublished at)) ->
                    respond resolver (published storage id at)

                Checked resolver token id (Just (Db.Waiting bytes)) ->
                    Clock (Timed resolver token id bytes)

                Timed resolver token id bytes time ->
                    Effect.map (Headed resolver token id bytes time) (R2.head storage time id)

                Headed resolver _ _ _ _ (Err reason) ->
                    storeFailed resolver reason

                Headed resolver _ _ _ _ (Ok Nothing) ->
                    respond resolver (Response.error 409 "The picture hasn't arrived. Upload it again.")

                Headed resolver token id bytes time (Ok (Just staged)) ->
                    if staged.bytes /= bytes || staged.bytes > Publication.maxBytes || staged.contentType /= "image/jpeg" then
                        Batch
                            [ log Interop.Warn
                                ("Picture "
                                    ++ PictureId.toString id
                                    ++ " arrived as "
                                    ++ String.fromInt staged.bytes
                                    ++ " bytes of "
                                    ++ String.left 40 staged.contentType
                                    ++ ", declared "
                                    ++ String.fromInt bytes
                                    ++ "."
                                )
                            , Effect.map (StagedRemoved resolver token id Refused) (R2.deleteStaged storage time id)
                            ]

                    else
                        Effect.map (Copied resolver token id time) (R2.copyToPublic storage time id staged.etag)

                Copied resolver token id time (Ok R2.Copied) ->
                    Effect.map (Cleaned resolver token id) (R2.deleteStaged storage time id)

                Copied resolver token id time (Ok R2.Changed) ->
                    Batch
                        [ log Interop.Warn ("Picture " ++ PictureId.toString id ++ " changed while publishing.")
                        , Effect.map (PublicRemoved resolver token id time Replaced) (R2.delete storage time id)
                        ]

                Copied resolver _ _ _ (Err reason) ->
                    storeFailed resolver reason

                Cleaned resolver token id result ->
                    Batch
                        [ case result of
                            Ok () ->
                                NoEffect

                            Err reason ->
                                log Interop.Warn ("Staged file left behind: " ++ reason)
                        , transact server.db resolver "Picture publish failed" (Db.markPublished token id) (Marked resolver token id)
                        ]

                Marked resolver _ id (Just at) ->
                    respond resolver (published storage id at)

                Marked resolver token id Nothing ->
                    transact server.db resolver "Picture lookup failed" (Db.unconfirmed token id) (Rechecked resolver id)

                Rechecked resolver id (Just (Db.AlreadyPublished at)) ->
                    respond resolver (published storage id at)

                Rechecked resolver _ _ ->
                    respond resolver (Response.error 409 "Publish it again.")

                PublicRemoved resolver token id time done (Ok ()) ->
                    Effect.map (StagedRemoved resolver token id done) (R2.deleteStaged storage time id)

                PublicRemoved resolver _ id _ _ (Err reason) ->
                    Batch
                        [ respond resolver (Response.error 503 "Couldn't store the picture.")
                        , log Interop.Error ("R2 delete failed for " ++ PictureId.toString id ++ ": " ++ reason)
                        ]

                StagedRemoved resolver token id done _ ->
                    transact server.db resolver "Picture removal failed" (Db.dropPicture token id) (Dropped resolver done)

                Dropped resolver Refused _ ->
                    respond resolver (Response.error 422 "The picture must be a JPEG of at most 2 MB.")

                Dropped resolver Replaced _ ->
                    respond resolver (Response.error 409 "The picture changed while publishing. Publish it again.")

                Dropped resolver Unpublished _ ->
                    respond resolver (Response.json 200 (Encode.object []))

                Owned resolver _ _ Nothing ->
                    respond resolver Response.notSignedIn

                Owned resolver _ _ (Just False) ->
                    respond resolver (Response.error 404 "No such picture.")

                Owned resolver token id (Just True) ->
                    Clock (Unpublishing resolver token id)

                Unpublishing resolver token id time ->
                    Effect.map (PublicDeleted resolver token id time) (R2.delete storage time id)

                PublicDeleted resolver token id time (Ok ()) ->
                    Effect.map (StagedRemoved resolver token id Unpublished) (R2.deleteStaged storage time id)

                PublicDeleted resolver _ _ _ (Err reason) ->
                    Batch
                        [ respond resolver (Response.error 503 "Couldn't remove the picture.")
                        , log Interop.Error ("R2 delete failed: " ++ reason)
                        ]

                Reported resolver _ _ _ ->
                    respond resolver Response.unavailable

                Notified resolver _ ->
                    respond resolver Response.unavailable


published : R2.Config -> PictureId -> String -> Response.Response
published storage id at =
    Response.json 200
        (Api.encodeResponse (Api.confirm id) { id = id, url = R2.publicUrl storage id, publishedAt = at })


storeFailed : Interop.Resolver -> String -> Effect msg
storeFailed resolver reason =
    Batch
        [ respond resolver (Response.error 503 "Couldn't store the picture.")
        , log Interop.Error ("R2 failed: " ++ reason)
        ]
