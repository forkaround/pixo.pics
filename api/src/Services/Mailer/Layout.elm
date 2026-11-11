module Services.Mailer.Layout exposing (Block(..), Message, minutes, render)


type alias Message =
    { subject : String
    , html : String
    , text : String
    }


type Block
    = Heading String
    | Paragraph String
    | Button { label : String, url : String }
    | Footnote String


render : { subject : String, preview : String, blocks : List Block } -> Message
render { subject, preview, blocks } =
    { subject = subject
    , html = document subject preview (String.concat (List.map blockToHtml blocks))
    , text = String.join "\n\n" (List.map blockToText blocks) ++ "\n"
    }


minutes : Int -> String
minutes count =
    if count == 1 then
        "1 minute"

    else
        String.fromInt count ++ " minutes"


document : String -> String -> String -> String
document subject preview content =
    String.concat
        [ "<!doctype html>"
        , "<html lang=\"en\">"
        , "<head>"
        , "<meta charset=\"utf-8\">"
        , "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
        , "<meta name=\"color-scheme\" content=\"light\">"
        , "<title>" ++ escape subject ++ "</title>"
        , "</head>"
        , "<body style=\"margin:0;padding:0;background-color:#f4f4f5;\">"
        , "<div style=\"display:none;max-height:0;overflow:hidden;opacity:0;\">" ++ escape preview ++ "</div>"
        , "<table role=\"presentation\" width=\"100%\" cellpadding=\"0\" cellspacing=\"0\" style=\"background-color:#f4f4f5;\">"
        , "<tr><td align=\"center\" style=\"padding:32px 16px;\">"
        , "<table role=\"presentation\" width=\"100%\" cellpadding=\"0\" cellspacing=\"0\" style=\"max-width:480px;background-color:#ffffff;border-radius:12px;\">"
        , "<tr><td style=\"padding:32px;font-family:" ++ fonts ++ ";color:#18181b;\">"
        , content
        , "</td></tr>"
        , "</table>"
        , "</td></tr>"
        , "</table>"
        , "</body>"
        , "</html>"
        ]


fonts : String
fonts =
    "-apple-system,BlinkMacSystemFont,'Segoe UI',Helvetica,Arial,sans-serif"


blockToHtml : Block -> String
blockToHtml block =
    case block of
        Heading text ->
            "<h1 style=\"margin:0 0 16px;font-size:22px;line-height:28px;font-weight:600;\">" ++ escape text ++ "</h1>"

        Paragraph text ->
            "<p style=\"margin:0 0 24px;font-size:16px;line-height:24px;\">" ++ escape text ++ "</p>"

        Button { label, url } ->
            String.concat
                [ "<p style=\"margin:0 0 24px;\">"
                , "<a href=\"" ++ escape url ++ "\" style=\"display:inline-block;padding:12px 20px;background-color:#18181b;color:#ffffff;border-radius:8px;font-size:16px;line-height:24px;font-weight:600;text-decoration:none;\">"
                , escape label
                , "</a>"
                , "</p>"
                , "<p style=\"margin:0 0 24px;font-size:13px;line-height:20px;color:#71717a;word-break:break-all;\">"
                , "Or open this link: <a href=\"" ++ escape url ++ "\" style=\"color:#71717a;\">" ++ escape url ++ "</a>"
                , "</p>"
                ]

        Footnote text ->
            "<p style=\"margin:0;font-size:13px;line-height:20px;color:#71717a;\">" ++ escape text ++ "</p>"


blockToText : Block -> String
blockToText block =
    case block of
        Heading text ->
            text

        Paragraph text ->
            text

        Button { label, url } ->
            label ++ ":\n" ++ url

        Footnote text ->
            text


escape : String -> String
escape =
    String.replace "&" "&amp;"
        >> String.replace "<" "&lt;"
        >> String.replace ">" "&gt;"
        >> String.replace "\"" "&quot;"
        >> String.replace "'" "&#39;"
