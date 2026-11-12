module Share exposing (Network(..), compose, name, networks)

import Url.Builder as Builder


type Network
    = X
    | Threads
    | Bluesky
    | LinkedIn
    | Reddit


networks : List Network
networks =
    [ X, Threads, Bluesky, LinkedIn, Reddit ]


name : Network -> String
name network =
    case network of
        X ->
            "X"

        Threads ->
            "Threads"

        Bluesky ->
            "Bluesky"

        LinkedIn ->
            "LinkedIn"

        Reddit ->
            "Reddit"


compose : Network -> String -> String
compose network text =
    case network of
        X ->
            Builder.crossOrigin "https://x.com" [ "intent", "post" ] [ Builder.string "text" text ]

        Threads ->
            Builder.crossOrigin "https://www.threads.com" [ "intent", "post" ] [ Builder.string "text" text ]

        Bluesky ->
            Builder.crossOrigin "https://bsky.app" [ "intent", "compose" ] [ Builder.string "text" text ]

        LinkedIn ->
            Builder.crossOrigin "https://www.linkedin.com"
                [ "feed", "" ]
                [ Builder.string "shareActive" "true", Builder.string "text" text ]

        Reddit ->
            Builder.crossOrigin "https://www.reddit.com"
                [ "submit" ]
                [ Builder.string "type" "IMAGE", Builder.string "title" text ]
