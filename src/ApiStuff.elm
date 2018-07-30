module ApiStuff exposing (..)

import Types exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, string, int, nullable, list)
import Json.Decode.Pipeline exposing (decode, required, optional)
import RemoteData exposing (WebData)

maybeGetShowArchive : Model -> Cmd Msg
maybeGetShowArchive model =
    case model.shows of
        RemoteData.NotAsked ->
            getPastShows model
        _ ->
            Cmd.none

getPastShows : Model -> Cmd Msg
getPastShows model =
    let
        url = model.config.api_url ++ "/shows/all"
    in
        Http.get url (list showDecoder)
            |> RemoteData.sendRequest
            |> Cmd.map ShowArchiveResponse

maybeGetShows : Model -> Cmd Msg
maybeGetShows model =
    case model.shows of
        RemoteData.NotAsked ->
            getShows model
        _ ->
            Cmd.none

getShows : Model -> Cmd Msg
getShows model =
    let
        url = model.config.api_url ++ "/shows"
    in
        Http.get url (list showDecoder)
            |> RemoteData.sendRequest
            |> Cmd.map ShowResponse

showDecoder : Decoder Show
showDecoder =
    decode Show
        |> required "id" int
        |> required "date" string
        |> required "venue" string
        |> required "address" string
        |> required "time" (nullable string)
        |> required "notes" (nullable string)
        |> required "links" (nullable string)

maybeGetNews : Model -> Cmd Msg
maybeGetNews model =
    case model.news of
        RemoteData.NotAsked ->
            getNews model
        _ ->
            Cmd.none

getNews : Model -> Cmd Msg
getNews model =
    let
        url = model.config.api_url ++ "/news"
    in
        Http.get url (list newsDecoder)
            |> RemoteData.sendRequest
            |> Cmd.map NewsResponse

newsDecoder : Decoder NewsEntry
newsDecoder =
    decode NewsEntry
        |> required "author" string
        |> required "content" string
        |> required "date" string
        |> required "id" int

maybeGetDiscog : Model -> Cmd Msg
maybeGetDiscog model =
    case model.releases of
        RemoteData.NotAsked ->
            getDiscog model
        _ ->
            Cmd.none

getDiscog : Model -> Cmd Msg
getDiscog model =
    let
        url = model.config.api_url ++ "/releases"
    in
        Http.get url (list discogDecoder)
            |> RemoteData.sendRequest
            |> Cmd.map DiscogResponse

discogDecoder : Decoder DiscogEntry
discogDecoder =
    decode DiscogEntry
        |> required "id" int
        |> required "name" string
        |> required "year" int
        |> required "imgsrc" string
        |> required "meta" string

getRelease : Model -> Int -> Cmd Msg
getRelease model id =
    let
        url = model.config.api_url ++ "/releases/" ++ (toString id)
    in
        Http.get url releaseDecoder
            |> RemoteData.sendRequest
            |> Cmd.map ReleaseResponse

releaseDecoder : Decoder Release
releaseDecoder =
    decode Release
        |> required "id" int
        |> required "name" string
        |> required "year" int
        |> required "label" (nullable string)
        |> required "format" (nullable string)
        |> required "recorded" (nullable string)
        |> required "mastered" (nullable string)
        |> required "story" (nullable string)
        |> required "tracklist" (nullable string)
        |> required "imgsrc" string
        |> required "meta" string
