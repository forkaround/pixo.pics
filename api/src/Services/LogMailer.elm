module Services.LogMailer exposing (email, warning)


email : { to : String, tag : String, subject : String, text : String } -> String
email { to, tag, subject, text } =
    String.join "\n"
        [ "Email not sent, printed instead (" ++ tag ++ ")"
        , "To: " ++ to
        , "Subject: " ++ subject
        , ""
        , text
        ]


warning : String
warning =
    "No BREVO_API_KEY, so emails are printed here instead of sent."
