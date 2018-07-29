module Main exposing (..)

import Http
import String
import List
import Html exposing (..)
import Html.Attributes exposing (src, href, class, classList, height, target)
import Html.Events exposing (onWithOptions)
import Navigation exposing (Location)
import Json.Decode as Decode exposing (Decoder, string, int, nullable, list)
import Json.Decode.Pipeline exposing (decode, required, optional)
import RemoteData exposing (WebData)

import Routing exposing (..)
import Types exposing (..)

initialModel : Config -> Route -> Model
initialModel config route =
    { path = ""
    , route = route
    , config = config
    , shows = RemoteData.NotAsked
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
        |> required "tracklist" string
        |> required "imgsrc" string
        |> required "meta" string

---- UPDATE ----

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange newLocation ->
            let
                newRoute = parseLocation newLocation
            in
                ({ model | path = newLocation.pathname, route = newRoute }, routeCmd model newRoute)
        NoOp ->
            ( model, Cmd.none)
        ChangeLocation path ->
            ( model, Navigation.newUrl path )
        ShowResponse shows ->
            ({ model | shows = shows}, Cmd.none)
        NewsResponse news ->
            ({ model | news = news}, Cmd.none)
        DiscogResponse releases ->
            ({ model | releases = releases}, Cmd.none)
        ReleaseResponse release ->
            ({ model | release = release}, Cmd.none)

routeCmd : Model -> Route -> Cmd Msg
routeCmd model route =
    case route of
        ShowsRoute ->
            maybeGetShows model
        NewsRoute ->
            maybeGetNews model
        NewsArchive _ ->
            maybeGetNews model
        DiscographyRoute ->
            maybeGetDiscog model
        ReleaseRoute id ->
            getRelease model id
        _ ->
            Cmd.none

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

isRoot : Model -> Bool
isRoot model =
    model.path == "/"

router : Model -> Html Msg
router model =
    case model.route of
        Root ->
            news model 0
        NewsRoute ->
            news model 0
        NewsArchive page ->
            news model page
        ShowsRoute ->
            shows model
        ShowArchiveRoute ->
            showArchive model
        StoreRoute ->
            store model
        DiscographyRoute ->
            releases model
        ReleaseRoute id ->
            release model
        AboutRoute ->
            about
        NotFound ->
            notFound
error : Html Msg
error =
  div [] [ text "Uh oh! Something went wrong. Email Jubert at "
        , a [ href "mailto:iamunclezappa@gmail.com"]
            [ text "iamunclezappa@gmail.com" ]
        , text " for tech support"
        ]
news : Model -> Int -> Html Msg
news model page =
    div [] [ text ("news. page " ++ toString page) ]

shows : Model -> Html Msg
shows model =
    div [] [ text "shows" ]

showArchive : Model -> Html Msg
showArchive model =
    div [] [ text "show archive"]

store : Model -> Html Msg
store model =
    div [] [ text "store" ]

releases : Model -> Html Msg
releases model =
    case model.releases of
        RemoteData.NotAsked ->
            div [] [ text "Getting releases..."]
        RemoteData.Loading ->
            div [] [ text "Getting releases..."]
        RemoteData.Failure _ ->
            error
        RemoteData.Success releases ->
            renderReleases releases

renderReleases : (List DiscogEntry) -> Html Msg
renderReleases releases =
    div []
        <| (List.map releaseThumb releases)

releaseThumb : DiscogEntry -> Html Msg
releaseThumb release =
    let
        url = "/releases/" ++ (toString release.id)
        name = release.name
        year = release.year
        imgsrc = release.imgsrc
    in
        a [ href url, onLinkClick (ChangeLocation url) ]
            [ div [ class "releaseThumb" ]
                [ text (name ++ " (" ++ (toString year) ++ ")")
                , br [] []
                , img [ class "releaseThumbnail"
                    , src imgsrc
                    ] []
                ]
            ]


release : Model -> Html Msg
release model =
    div [] [ text "one release" ]

about : Html Msg
about =
    div [] [ text "about" ]

notFound : Html Msg
notFound =
    div [] [ text "error 404 not found bro" ]

splash : Html Msg
splash =
    div [ class "splash" ]
        [
            a [ href "/news", onLinkClick (ChangeLocation "/news")]
              [ img [ src "/billysfriend.jpg", height 500 ] [] ]
        ]

home : Model -> Html Msg
home model =
    div [ class "home" ]
        [ nav model
        -- , footer
        , div [ class "content" ] [ router model ]
        ]

footerSpacer : Html Msg
footerSpacer =
    span [] [text " -x- " ]

footerLink : String -> String -> Html Msg
footerLink url title =
    a [target "_blank", class "link", href url] [text title]

footer : Html Msg
footer =
    div [ class "footer" ]
    [ div [ class "footSubDiv" ]
          [ img [class "jubertFooter", src "/jubertLeft.png" ] [] ]
    , div [ class "footSubDiv" ]
        [ footerLink "https://cokebust.bandcamp.com/" "bandcamp", footerSpacer
        , footerLink "https://twitter.com/cokebust?lang=en" "twitter", footerSpacer
        , footerLink "https://www.facebook.com/cokebust/" "facebook", footerSpacer
        , footerLink "mailto:cokebust@gmail.com" "cokebust@gmail.com"
        ]
    , div [ class "footSubDiv" ]
          [ img [ class "jubertFooter", src "/jubertRight.png" ] [] ]
    ]

nav : Model -> Html Msg
nav model =
    div [ class "nav" ]
        [ img [ src "/logo.jpg", class "navLogo"] []
        , navItem "News" model
        , navItem "Shows" model
        , navItem "Store" model
        , navItem "Releases" model
        , navItem "About" model
        ]

navItem : String -> Model -> Html Msg
navItem title model =
    let
        url = "/" ++ String.toLower title

    in
        div [ classList
                [ ("navItem", True)
                , ("navActive", String.startsWith url model.path)
                ]
            ]
            [ a [ href url, onLinkClick (ChangeLocation url) ] [ text title]]

onLinkClick : msg -> Attribute msg
onLinkClick message =
    let
        options =
            { stopPropagation = False
            , preventDefault = True
            }
    in
        onWithOptions
            "click"
            options
            (preventDefaultUnlessKeyPressed
                |> Decode.andThen (maybePreventDefault message)
            )

preventDefaultUnlessKeyPressed : Decode.Decoder Bool
preventDefaultUnlessKeyPressed =
    Decode.map2
        nor
        (Decode.field "ctrlKey" Decode.bool)
        (Decode.field "metaKey" Decode.bool)

nor : Bool -> Bool -> Bool
nor x y =
    not (x || y)

maybePreventDefault : msg -> Bool -> Decode.Decoder msg
maybePreventDefault msg preventDefault =
    case preventDefault of
        True ->
            Decode.succeed msg

        False ->
            Decode.fail "Delegated to browser default"
