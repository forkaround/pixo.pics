module Flow.Feed exposing (Msg, feed, profile, tags, update)

import Api
import Data.PictureId as PictureId exposing (PictureId)
import Data.Tag exposing (Tag)
import Data.Username exposing (Username)
import Db
import Effect exposing (Effect, query, respond)
import InteropDefinitions as Interop
import Reply exposing (Reply(..))
import Request exposing (Request)
import Response
import Server exposing (Server)
import Services.R2 as R2


type Msg
    = GotFeed Interop.Resolver (Maybe Tag) (Maybe PictureId) (List Db.PictureRow)
    | GotTags Interop.Resolver (List Api.TagCount)
    | GotProfile Interop.Resolver Username (Maybe PictureId) (Maybe Db.ProfileRow)


pageSize : Int
pageSize =
    30


feed : Maybe Tag -> Maybe PictureId -> Server -> Request -> Reply Msg
feed tag before server _ =
    case server.storage of
        Nothing ->
            Now Response.unavailable

        Just _ ->
            Later
                (\resolver ->
                    query server.db resolver "Feed read failed" (Db.feed tag before pageSize) (GotFeed resolver tag before)
                )


tags : Server -> Request -> Reply Msg
tags server _ =
    Later
        (\resolver ->
            query server.db resolver "Feed tags read failed" (Db.feedTags 20) (GotTags resolver)
        )


profile : Username -> Maybe PictureId -> Server -> Request -> Reply Msg
profile name before server _ =
    case server.storage of
        Nothing ->
            Now Response.unavailable

        Just _ ->
            Later
                (\resolver ->
                    query server.db resolver "Profile read failed" (Db.profile name before pageSize) (GotProfile resolver name before)
                )


update : Server -> Msg -> Effect Msg
update server msg =
    case ( server.storage, msg ) of
        ( _, GotTags resolver found ) ->
            respond resolver (Response.shared 300 (Api.encodeResponse Api.feedTags found))

        ( Nothing, GotFeed resolver _ _ _ ) ->
            respond resolver Response.unavailable

        ( Nothing, GotProfile resolver _ _ _ ) ->
            respond resolver Response.unavailable

        ( Just storage, GotFeed resolver tag before rows ) ->
            respond resolver
                (Response.shared 60
                    (Api.encodeResponse (Api.feed tag before)
                        { pictures = List.map (picture storage) rows
                        , next = next rows
                        }
                    )
                )

        ( Just _, GotProfile resolver _ _ Nothing ) ->
            respond resolver (Response.error 404 "No such profile.")

        ( Just storage, GotProfile resolver name before (Just row) ) ->
            respond resolver
                (Response.shared 60
                    (Api.encodeResponse (Api.profile name before)
                        { username = row.username
                        , pictures = List.map (picture storage) row.pictures
                        , next = next row.pictures
                        }
                    )
                )


next : List Db.PictureRow -> Maybe PictureId
next rows =
    if List.length rows >= pageSize then
        rows
            |> List.reverse
            |> List.head
            |> Maybe.map .id

    else
        Nothing


picture : R2.Config -> Db.PictureRow -> Api.FeedPicture
picture storage row =
    { id = row.id
    , url = R2.publicUrl storage row.id
    , username = row.username
    , prompt = row.prompt
    , width = row.width
    , height = row.height
    , steps = row.steps
    , seed = row.seed
    , model = row.model
    , tags = row.tags
    , publishedAt = row.publishedAt
    }
