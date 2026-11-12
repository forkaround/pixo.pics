module ShareTest exposing (suite)

import Expect
import Share exposing (Network(..))
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Share"
        [ test "opens each network's own composer" <|
            \() ->
                List.map (\network -> Share.compose network "a fox") Share.networks
                    |> Expect.equal
                        [ "https://x.com/intent/post?text=a%20fox"
                        , "https://www.threads.com/intent/post?text=a%20fox"
                        , "https://bsky.app/intent/compose?text=a%20fox"
                        , "https://www.linkedin.com/feed/?shareActive=true&text=a%20fox"
                        , "https://www.reddit.com/submit?type=IMAGE&title=a%20fox"
                        ]
        , test "keeps a prompt from changing the address" <|
            \() ->
                Share.compose X "cats & dogs? 100% #1"
                    |> Expect.equal "https://x.com/intent/post?text=cats%20%26%20dogs%3F%20100%25%20%231"
        ]
