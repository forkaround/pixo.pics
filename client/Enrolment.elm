module Enrolment exposing (Msg, Outcome(..), created, failed, requested, update)

import Api
import ApiClient
import Ceremony exposing (Ceremony(..))
import Effect exposing (Effect(..))
import InteropDefinitions as Interop


type Msg
    = Requested
    | GotOptions (Result ApiClient.Error Api.PasskeyOptions)
    | Created Api.Attestation
    | Failed String
    | GotPasskey (Result ApiClient.Error Api.Passkey)


type Outcome
    = Pending
    | Enrolled Api.Passkey


requested : Msg
requested =
    Requested


created : Api.Attestation -> Msg
created =
    Created


failed : String -> Msg
failed =
    Failed


update : Msg -> Ceremony -> ( Ceremony, Effect Msg, Outcome )
update msg ceremony =
    case msg of
        Requested ->
            if ceremony == Waiting then
                ( ceremony, NoEffect, Pending )

            else
                ( Waiting, Request (ApiClient.request Api.passkeyOptions () GotOptions), Pending )

        GotOptions (Ok options) ->
            ( ceremony, SendToJs (Interop.CreatePasskey options), Pending )

        GotOptions (Err error) ->
            ( Resting (Just (ApiClient.errorToString error)), NoEffect, Pending )

        Created attestation ->
            if ceremony == Waiting then
                ( ceremony, Request (ApiClient.request Api.addPasskey attestation GotPasskey), Pending )

            else
                ( ceremony, NoEffect, Pending )

        Failed name ->
            ( Resting (Just (problem name)), NoEffect, Pending )

        GotPasskey (Ok passkey) ->
            ( Resting Nothing, NoEffect, Enrolled passkey )

        GotPasskey (Err error) ->
            ( Resting (Just (ApiClient.errorToString error)), NoEffect, Pending )


problem : String -> String
problem name =
    case name of
        "NotAllowedError" ->
            "The passkey was not created."

        "InvalidStateError" ->
            "This device already has a passkey for this account."

        "NotSupportedError" ->
            "This browser can't create passkeys."

        _ ->
            "The passkey could not be created."
