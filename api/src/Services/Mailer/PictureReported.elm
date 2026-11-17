module Services.Mailer.PictureReported exposing (email)

import Services.Mailer.Layout as Layout exposing (Message)


email : { url : String, profile : String, reason : String, pictureId : String } -> Message
email { url, profile, reason, pictureId } =
    Layout.render
        { subject = "A picture was reported on pixo"
        , preview = "Reason: " ++ reason
        , blocks =
            [ Layout.Heading "A picture was reported"
            , Layout.Paragraph ("Reason: " ++ reason ++ ".")
            , Layout.Button { label = "See the picture", url = url }
            , Layout.Paragraph ("Profile: " ++ profile)
            , Layout.Footnote ("Take it down from Neon's SQL editor: SELECT * FROM take_down_picture('" ++ pictureId ++ "'); then follow docs/agents/moderation.md.")
            ]
        }
