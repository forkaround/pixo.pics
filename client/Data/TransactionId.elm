module Data.TransactionId exposing (TransactionId, fromString, toString)


type TransactionId
    = TransactionId String


fromString : String -> Maybe TransactionId
fromString raw =
    case String.split "_" raw of
        [ "txn", rest ] ->
            if String.length rest >= 1 && String.length rest <= 64 && String.all isIdCharacter rest then
                Just (TransactionId raw)

            else
                Nothing

        _ ->
            Nothing


isIdCharacter : Char -> Bool
isIdCharacter character =
    Char.isDigit character || (Char.isLower character && Char.toCode character < 128)


toString : TransactionId -> String
toString (TransactionId raw) =
    raw
