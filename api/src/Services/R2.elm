module Services.R2 exposing (Config, Copied(..), Staged, config, copyToPublic, delete, deleteStaged, head, publicKey, publicUrl, stagedKey, uploadLink)

import Data.PictureId as PictureId exposing (PictureId)
import Dict
import Effect exposing (Effect)
import Http
import SigV4
import Time


type alias Config =
    { accountId : String
    , accessKeyId : String
    , secretAccessKey : String
    , bucket : String
    , uploadBucket : String
    , publicOrigin : String
    }


type alias Staged =
    { bytes : Int
    , contentType : String
    , etag : String
    }


type Copied
    = Copied
    | Changed


config :
    { a
        | r2AccountId : Maybe String
        , r2AccessKeyId : Maybe String
        , r2SecretAccessKey : Maybe String
        , r2Bucket : Maybe String
        , r2UploadBucket : Maybe String
        , imagesOrigin : Maybe String
    }
    -> Result String (Maybe Config)
config flags =
    if List.all ((==) Nothing) [ flags.r2AccountId, flags.r2AccessKeyId, flags.r2SecretAccessKey, flags.r2Bucket, flags.r2UploadBucket, flags.imagesOrigin ] then
        Ok Nothing

    else
        case
            Maybe.map5
                (\accountId accessKeyId secretAccessKey bucket uploadBucket ->
                    { accountId = accountId
                    , accessKeyId = accessKeyId
                    , secretAccessKey = secretAccessKey
                    , bucket = bucket
                    , uploadBucket = uploadBucket
                    }
                )
                flags.r2AccountId
                flags.r2AccessKeyId
                flags.r2SecretAccessKey
                flags.r2Bucket
                flags.r2UploadBucket
                |> Maybe.andThen (\set -> Maybe.map (\origin -> ( set, origin )) flags.imagesOrigin)
        of
            Just ( set, origin ) ->
                if String.startsWith "https://" origin || String.startsWith "http://localhost" origin then
                    Ok
                        (Just
                            { accountId = set.accountId
                            , accessKeyId = set.accessKeyId
                            , secretAccessKey = set.secretAccessKey
                            , bucket = set.bucket
                            , uploadBucket = set.uploadBucket
                            , publicOrigin = withoutTrailingSlash origin
                            }
                        )

                else
                    Err problem

            Nothing ->
                Err problem


problem : String
problem =
    "R2_ACCOUNT_ID, R2_ACCESS_KEY_ID, R2_SECRET_ACCESS_KEY, R2_BUCKET, R2_UPLOAD_BUCKET and IMAGES_ORIGIN must all be set, or none."


withoutTrailingSlash : String -> String
withoutTrailingSlash text =
    if String.endsWith "/" text then
        withoutTrailingSlash (String.dropRight 1 text)

    else
        text


publicKey : PictureId -> String
publicKey id =
    PictureId.toString id ++ ".jpg"


stagedKey : PictureId -> String
stagedKey id =
    "incoming/" ++ PictureId.toString id ++ ".jpg"


publicUrl : Config -> PictureId -> String
publicUrl c id =
    c.publicOrigin ++ "/" ++ publicKey id


host : Config -> String
host c =
    c.accountId ++ ".r2.cloudflarestorage.com"


credentials : Config -> SigV4.Credentials
credentials c =
    { accessKeyId = c.accessKeyId
    , secretAccessKey = c.secretAccessKey
    , region = "auto"
    , service = "s3"
    }


timeout : Float
timeout =
    10000


jpeg : ( String, String )
jpeg =
    ( "content-type", "image/jpeg" )


uploadLink : Config -> Time.Posix -> PictureId -> { url : String, headers : List ( String, String ) }
uploadLink c time id =
    { url =
        SigV4.presign (credentials c)
            time
            { method = "PUT"
            , host = host c
            , path = c.uploadBucket :: String.split "/" (stagedKey id)
            , headers = [ jpeg ]
            , expires = 600
            }
    , headers = [ jpeg ]
    }


head : Config -> Time.Posix -> PictureId -> Effect (Result String (Maybe Staged))
head c time id =
    send c time "HEAD" c.uploadBucket (stagedKey id) [] headOutcome


copyToPublic : Config -> Time.Posix -> PictureId -> String -> Effect (Result String Copied)
copyToPublic c time id etag =
    send c
        time
        "PUT"
        c.bucket
        (publicKey id)
        [ ( "cache-control", "public, max-age=3600" )
        , jpeg
        , ( "x-amz-copy-source", "/" ++ c.uploadBucket ++ "/" ++ stagedKey id )
        , ( "x-amz-copy-source-if-match", etag )
        , ( "x-amz-metadata-directive", "REPLACE" )
        ]
        (copyOutcome etag)


deleteStaged : Config -> Time.Posix -> PictureId -> Effect (Result String ())
deleteStaged c time id =
    send c time "DELETE" c.uploadBucket (stagedKey id) [] deleteOutcome


delete : Config -> Time.Posix -> PictureId -> Effect (Result String ())
delete c time id =
    send c time "DELETE" c.bucket (publicKey id) [] deleteOutcome


send : Config -> Time.Posix -> String -> String -> String -> List ( String, String ) -> (Http.Response String -> Result String a) -> Effect (Result String a)
send c time method bucket key headers onResponse =
    Effect.Http
        { method = method
        , headers =
            SigV4.sign (credentials c)
                time
                { method = method
                , host = host c
                , path = bucket :: String.split "/" key
                , query = []
                , headers = headers
                , payloadHash = SigV4.unsignedPayload
                }
        , url = "https://" ++ host c ++ "/" ++ bucket ++ "/" ++ key
        , body = Effect.EmptyBody
        , timeout = Just timeout
        , onResponse = onResponse
        }


headOutcome : Http.Response String -> Result String (Maybe Staged)
headOutcome response =
    case response of
        Http.GoodStatus_ metadata _ ->
            case
                ( Dict.get "content-length" metadata.headers |> Maybe.andThen String.toInt
                , Dict.get "content-type" metadata.headers
                , Dict.get "etag" metadata.headers
                )
            of
                ( Just bytes, Just contentType, Just etag ) ->
                    Ok (Just { bytes = bytes, contentType = contentType, etag = etag })

                _ ->
                    Err "R2 answered a HEAD without a size, a type or an ETag."

        Http.BadStatus_ metadata body ->
            if metadata.statusCode == 404 then
                Ok Nothing

            else
                Err (refused metadata.statusCode body)

        _ ->
            Err (unreachable response)


copyOutcome : String -> Http.Response String -> Result String Copied
copyOutcome wanted response =
    case response of
        Http.GoodStatus_ metadata body ->
            if String.contains "<Error>" body then
                Err (refused metadata.statusCode body)

            else
                case elementOf "ETag" body of
                    Just returned ->
                        if bare returned == bare wanted then
                            Ok Copied

                        else
                            Ok Changed

                    Nothing ->
                        Err (refused metadata.statusCode body)

        Http.BadStatus_ metadata body ->
            if metadata.statusCode == 412 then
                Ok Changed

            else
                Err (refused metadata.statusCode body)

        _ ->
            Err (unreachable response)


deleteOutcome : Http.Response String -> Result String ()
deleteOutcome response =
    case response of
        Http.GoodStatus_ _ _ ->
            Ok ()

        Http.BadStatus_ metadata body ->
            if metadata.statusCode == 404 then
                Ok ()

            else
                Err (refused metadata.statusCode body)

        _ ->
            Err (unreachable response)


elementOf : String -> String -> Maybe String
elementOf name body =
    case String.split ("<" ++ name ++ ">") body of
        _ :: rest :: _ ->
            String.split ("</" ++ name ++ ">") rest
                |> List.head

        _ ->
            Nothing


bare : String -> String
bare etag =
    etag
        |> String.replace "&quot;" ""
        |> String.replace "\"" ""


refused : Int -> String -> String
refused status body =
    "R2 answered " ++ String.fromInt status ++ ": " ++ String.left 200 body


unreachable : Http.Response String -> String
unreachable response =
    case response of
        Http.Timeout_ ->
            "R2 took too long to answer."

        Http.BadUrl_ url ->
            "Bad R2 endpoint: " ++ url

        _ ->
            "Could not reach R2."
