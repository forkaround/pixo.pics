module Idb exposing
    ( Command(..)
    , Database
    , Event(..)
    , Store
    )

import Json.Encode exposing (Value)


type alias Database =
    { name : String
    , version : Int
    , stores : List Store
    }


type alias Store =
    { name : String
    , keyPath : String
    , autoIncrement : Bool
    }


type Command
    = GetAll { database : Database, store : String, label : String }
    | Add
        { database : Database
        , store : String
        , row : List ( String, Value )
        , blobs : List ( String, String )
        , label : String
        }
    | Delete { database : Database, store : String, key : Value, label : String }
    | Release (List String)


type Event
    = Rows { label : String, rows : List Value }
    | Added { label : String, row : Value }
    | Deleted { label : String, key : Value }
    | Failed { label : String, message : String }
