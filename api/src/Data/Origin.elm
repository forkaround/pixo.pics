module Data.Origin exposing (Origin, href, isSecure, parse, rpId, toString)

import Path exposing (Path)
import Url exposing (Url)


type Origin
    = Origin
        { url : String
        , rpId : String
        , secure : Bool
        }


parse : String -> Result String Origin
parse value =
    case Url.fromString value of
        Just url ->
            if url.path == "/" && url.query == Nothing && url.fragment == Nothing then
                Ok (fromUrl url)

            else
                Err ("Expected an origin without a path, like https://pixo.pics, got: " ++ value)

        Nothing ->
            Err ("Expected an origin like https://pixo.pics, got: " ++ value)


fromUrl : Url -> Origin
fromUrl url =
    let
        host =
            String.toLower url.host

        secure =
            url.protocol == Url.Https

        ( scheme, defaultPort ) =
            if secure then
                ( "https://", 443 )

            else
                ( "http://", 80 )

        suffix =
            case url.port_ of
                Just number ->
                    if number == defaultPort then
                        ""

                    else
                        ":" ++ String.fromInt number

                Nothing ->
                    ""
    in
    Origin
        { url = scheme ++ host ++ suffix
        , rpId = host
        , secure = secure
        }


toString : Origin -> String
toString (Origin origin) =
    origin.url


rpId : Origin -> String
rpId (Origin origin) =
    origin.rpId


isSecure : Origin -> Bool
isSecure (Origin origin) =
    origin.secure


href : Origin -> Path -> String
href origin path =
    toString origin ++ Path.toString path
