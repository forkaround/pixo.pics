module Enrolment exposing (Msg, Outcome(..), created, failed, requested, toast, update)

import Api
import ApiClient
import Ceremony exposing (Ceremony(..))
import Effect exposing (Effect(..))
import InteropDefinitions as Interop
import Notice exposing (Notice)


type Msg
    = Requested
    | Retried
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
                ( Waiting, askOptions, Pending )

        Retried ->
            if ceremony == Waiting then
                ( ceremony, askOptions, Pending )

            else
                ( ceremony, NoEffect, Pending )

        GotOptions (Ok options) ->
            ( ceremony, SendToJs (Interop.CreatePasskey options), Pending )

        GotOptions (Err error) ->
            refused error ceremony

        Created attestation ->
            if ceremony == Waiting then
                ( ceremony, Request (ApiClient.request Api.addPasskey attestation GotPasskey), Pending )

            else
                ( ceremony, NoEffect, Pending )

        Failed name ->
            ( Resting, Notify (problem name), Pending )

        GotPasskey (Ok passkey) ->
            ( Resting, Dismiss toast, Enrolled passkey )

        GotPasskey (Err error) ->
            refused error ceremony


toast : String
toast =
    "passkey-creation"


title : String
title =
    "Couldn't create a passkey"


askOptions : Effect Msg
askOptions =
    Request (ApiClient.request Api.passkeyOptions () GotOptions)


refused : ApiClient.Error -> Ceremony -> ( Ceremony, Effect Msg, Outcome )
refused error ceremony =
    case error of
        ApiClient.Throttled _ _ ->
            ( ceremony, Effect.failure { id = toast, title = title, retry = Just Retried } error, Pending )

        _ ->
            ( Resting, Effect.failure { id = toast, title = title, retry = Just Requested } error, Pending )


problem : String -> Notice Msg
problem name =
    { id = toast
    , tone = Notice.Problem
    , title = title
    , description = Just (reason name)
    , retry =
        case name of
            "InvalidStateError" ->
                Notice.NoRetry

            "NotSupportedError" ->
                Notice.NoRetry

            _ ->
                Notice.Retry Requested
    }


reason : String -> String
reason name =
    case name of
        "NotAllowedError" ->
            "The passkey was not created."

        "InvalidStateError" ->
            "This device already has a passkey for this account."

        "NotSupportedError" ->
            "This browser can't create passkeys."

        _ ->
            "The passkey could not be created."
