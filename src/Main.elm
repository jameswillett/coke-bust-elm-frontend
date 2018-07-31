port module Main exposing (..)

import Html exposing (..)
import Navigation exposing (Location)
import RemoteData
import Routing exposing (parseLocation)
import Types exposing (..)
import ApiStuff exposing (..)
import Views exposing (isRoot, splash, home)

---- PORTS ----

port title : String -> Cmd msg
port openStore : String -> Cmd msg

---- INIT ----

initialModel : Config -> Route -> Model
initialModel config route =
    { path = ""
    , route = route
    , config = config
    , shows = RemoteData.NotAsked
    , pastShows = RemoteData.NotAsked
    , news = RemoteData.NotAsked
    , releases = RemoteData.NotAsked
    , release = RemoteData.NotAsked
    }

init : Config -> Location -> ( Model, Cmd Msg )
init config location =
    let
        currentRoute = parseLocation location
    in
        (initialModel config currentRoute) |> update ( UrlChange location )


---- UPDATE ----

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange newLocation ->
            let
                newRoute = parseLocation newLocation
            in
                ({ model |
                      path = newLocation.pathname
                      , route = newRoute
                  }, Cmd.batch [ getTitle newRoute |> title, routeCmd model newRoute ])
        NoOp ->
            ( model, Cmd.none)
        ChangeLocation path ->
            ( model, Navigation.newUrl path )
        ShowResponse shows ->
            ({ model | shows = shows}, Cmd.none)
        ShowArchiveResponse pastShows ->
            ({ model | pastShows = pastShows}, Cmd.none)
        NewsResponse news ->
            ({ model | news = news}, Cmd.none)
        DiscogResponse releases ->
            ({ model | releases = releases}, Cmd.none)
        ReleaseResponse release ->
            ({ model | release = release }
              , Cmd.batch [ getTitleFromRelease release |> title, maybeGetDiscog model ])

routeCmd : Model -> Route -> Cmd Msg
routeCmd model route =
    case route of
        ShowsRoute ->
            maybeGetShows model
        ShowArchiveRoute ->
            maybeGetShowArchive model
        NewsRoute ->
            maybeGetNews model
        NewsArchive _ ->
            maybeGetNews model
        DiscographyRoute ->
            maybeGetDiscog model
        ReleaseRoute id ->
            getRelease model id
        StoreRoute ->
            openStore "jubert has big dick energy"
        _ ->
            Cmd.none

getTitleFromRelease : (RemoteData.WebData Release) -> String
getTitleFromRelease release =
    case release of
        RemoteData.Success x ->
            "Coke Bust - " ++ x.name
        _ ->
            "Coke Bust - Loading..."

getTitle : Route -> String
getTitle route =
    case route of
        Root ->
            "Coke Bust"
        NewsRoute ->
            "Coke Bust - News"
        NewsArchive _ ->
            "Coke Bust - News"
        ShowsRoute ->
            "Coke Bust - Upcoming Shows"
        ShowArchiveRoute ->
            "Coke Bust - Show Archive"
        StoreRoute ->
            "Coke Bust - Store"
        DiscographyRoute ->
            "Coke Bust - Releases"
        ReleaseRoute _ ->
            "Coke Bust - Loading..."
        AboutRoute ->
            "Coke Bust - About"
        NotFound ->
            "Coke Bust - 404"

---- VIEW ----


view : Model -> Html Msg
view model =
    if (isRoot model) then splash else (home model)


---- PROGRAM ----


main : Program Config Model Msg
main =
    Navigation.programWithFlags UrlChange
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
