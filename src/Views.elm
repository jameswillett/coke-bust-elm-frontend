module Views exposing (isRoot, splash, home)

import String
import List
import Html exposing (..)
import Html.Attributes exposing (src, href, class, classList, height, target, property, style)
import Json.Encode
import RemoteData
import Date exposing (Date)
import Date.Extra as Date
import Result exposing (andThen, toMaybe)
import Types exposing (..)
import NavTo exposing (navTo)

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

---- NEWS STUFF ----

prevNext : (List NewsEntry) -> Int -> Html Msg
prevNext news page =
    let
        prevUrl = if page == 1 then "/news" else "/news/" ++ (page - 1 |> toString)
        nextUrl = "/news/" ++ (page + 1 |> toString)
    in
        div [ class "moreLess"]
            [ navTo prevUrl [
                classList [("unclicky", page == 0)]
            ] [ text "Previous" ]
            , span [] [ text " || "]
            , navTo nextUrl [
                classList [("unclicky", (page + 1 |> (*) 5) > (List.length news))]
            ] [ text "Next" ]
            ]


monthToNumber : Date.Month -> String
monthToNumber month =
    case month of
        Date.Jan ->
            "1"
        Date.Feb ->
            "2"
        Date.Mar ->
            "3"
        Date.Apr ->
            "4"
        Date.May ->
            "5"
        Date.Jun ->
            "6"
        Date.Jul ->
            "7"
        Date.Aug ->
            "8"
        Date.Sep ->
            "9"
        Date.Oct ->
            "10"
        Date.Nov ->
            "11"
        Date.Dec ->
            "12"

renderDate : Date.Date -> String
renderDate date =
    let
        month = Date.month date |> monthToNumber
        day = Date.day date |> toString
        year = Date.year date |> toString |> String.dropLeft 2
    in
        month ++ "/" ++ day ++ "/" ++ year

extractDate : String -> (Maybe Date.Date) -> String
extractDate original res =
    case res of
        Just x ->
            renderDate x
        _ ->
            original

parseDate : String -> String
parseDate dateString =
    toMaybe (Date.fromIsoString dateString) |> (extractDate dateString)

getPageOfNews : (List NewsEntry) -> Int -> (List NewsEntry)
getPageOfNews news page =
    List.drop (page * 5) news
        |> List.take 5

renderNewsEntry : NewsEntry -> Html Msg
renderNewsEntry entry =
    let
        innerHtml = Json.Encode.string entry.content
    in
      tr []
          [ td [ class "info" ] [ (parseDate entry.date) ++ " by " ++ entry.author |> text]
          , td [ class "entry" ] [ span [ property "innerHTML" innerHtml  ] [] ]
          ]

renderNews : (List NewsEntry) -> Int -> Html Msg
renderNews news page =
    let
        pageOfNews = getPageOfNews news page
    in
        div []
            [ List.map renderNewsEntry pageOfNews |> table [ class "news" ]
            , prevNext news page
            ]


news : Model -> Int -> Html Msg
news model page =
    case model.news of
        RemoteData.NotAsked ->
            div [] [ text "Getting news..."]
        RemoteData.Loading ->
            div [] [ text "Getting news..."]
        RemoteData.Failure _ ->
            error
        RemoteData.Success news ->
            renderNews news page

---- SHOWS STUFF ----

maybeTime : (Maybe String) -> String
maybeTime time =
    case time of
        Just x ->
            x
        _ ->
            "?"

maybeNotes : (Maybe String) -> String
maybeNotes notes =
    case notes of
        Just x ->
            x
        _ ->
            ""

maybeLink : (Maybe String) -> Html Msg
maybeLink link =
    case link of
        Just x ->
            a [ href x] [ text "XXX"]
        _ ->
            span [] []

renderShowRow : Show -> Html Msg
renderShowRow show =
    tr [ Html.Attributes.id ("showid-" ++ (toString show.id))]
        [ td [ class "showInfo" ] [ text (parseDate show.date) ]
        , td [ class "showInfo" ] [ text show.venue ]
        , td [ class "showInfo" ] [ text show.address ]
        , td [ class "showInfo" ] [ text (maybeTime show.time) ]
        , td [ class "showInfo" ] [ text (maybeNotes show.notes) ]
        , td [ class "showInfo" ] [ maybeLink show.links ]
        ]

renderShows : String -> (List Show) -> Html Msg
renderShows title shows =
    div []
        [ h2 [] [ text title ]
        , case (List.length shows) of
            0 ->
                p [] [ text "No Upcoming shows"]
            _ ->
                table [ class "shows" ]
                    (tr []
                        [ th [] [ text "Date" ]
                        , th [] [ text "Venue" ]
                        , th [] [ text "Address" ]
                        , th [] [ text "Time" ]
                        , th [] [ text "Notes" ]
                        , th [] [ text "FB event Page" ]
                        ] :: (List.map renderShowRow shows))
        ]




shows : Model -> Html Msg
shows model =
    div []
        [ case model.shows of
            RemoteData.NotAsked ->
                div [] [ text "Getting Shows..."]
            RemoteData.Loading ->
                div [] [ text "Getting Shows..."]
            RemoteData.Failure _ ->
                error
            RemoteData.Success shows ->
                renderShows "Upcoming Shows" shows
        , div [] [ navTo "/shows/all" [] [ text "View show archive"]]
        ]

showArchive : Model -> Html Msg
showArchive model =
    div []
        [ case model.pastShows of
            RemoteData.NotAsked ->
                div [] [ text "Getting Show Archive..."]
            RemoteData.Loading ->
                div [] [ text "Getting Show Archive..."]
            RemoteData.Failure _ ->
                error
            RemoteData.Success pastShows ->
                renderShows "Show Archive" (List.reverse pastShows)
        ]

---- STORE ----

store : Model -> Html Msg
store model =
    div [] [ p [] [ text "Store maybe coming soon. Forwarding you to our storenvy. Buy some jewelry while you're there!" ]
            , p [] [ a [ target "_blank"
                        , href "http://newroseaccessories.storenvy.com/collections/1679757-coke-bust"
                        ]
                        [ text "Click here if not forwarded in 5 seconds"]
                    ]
            ]
    -- div []
    --     [ iframe
    --           [ src "http://newroseaccessories.storenvy.com/collections/1679757-coke-bust" ] [ text "x" ]]

---- DISCOG PAGE STUFF ----

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
        navTo url []
            [ div [ class "releaseThumb" ]
                [ text (name ++ " (" ++ (toString year) ++ ")")
                , br [] []
                , img [ class "releaseThumbnail"
                    , src imgsrc
                    ] []
                ]
            ]

---- RELEASE PAGE STUFF ----

renderOther : DiscogEntry -> Html Msg
renderOther other =
    li [] [ navTo ("/releases/" ++ (toString other.id)) [] [ text other.name ]]

renderOthers : (List DiscogEntry) -> Html Msg
renderOthers others =
    case (others |> List.length) of
        0 ->
            span [] []
        _ ->
            div []
                [ text "Other Versions"
                , ul [] (List.map renderOther others)
                ]

otherVersions : Model -> Release -> Html Msg
otherVersions model release =
    case model.releases of
        RemoteData.NotAsked ->
            div [] [ text "Getting other versions..."]
        RemoteData.Loading ->
            div [] [ text "Getting other versions..."]
        RemoteData.Failure _ ->
            error
        RemoteData.Success releases ->
            let
                others = List.filter (\x -> x.meta == release.meta && x.id /= release.id) releases
            in
                renderOthers others

maybeRenderStory : Release -> Html Msg
maybeRenderStory release =
    case release.story of
        Just story ->
            div [ class "releaseInfo"] [ text story ]
        _ ->
            span [] []

songLi : String -> Html Msg
songLi s =
    li [] [ text s ]

maybeTracklist : Release -> Html Msg
maybeTracklist release =
    case release.tracklist of
        Just tracklist ->
            ol [ class "tracklist" ]
                (String.split "," tracklist |> List.map songLi)
        _ ->
            span [] []

maybeLabel : Release -> Html Msg
maybeLabel release =
    case release.label of
        Just label ->
            div [] [ "Label: " ++ label |> text]
        _ ->
            span [] []

maybeFormat : Release -> Html Msg
maybeFormat release =
    case release.format of
        Just format ->
            div [] [ "Format: " ++ format |> text]
        _ ->
            span [] []

maybeRecorded : Release -> Html Msg
maybeRecorded release =
    case release.recorded of
        Just recorded ->
            div [] [ "Recorded: " ++ recorded |> text]
        _ ->
            span [] []

maybeMastered : Release -> Html Msg
maybeMastered release =
    case release.mastered of
        Just mastered ->
            div [] [ "Mastered: " ++ mastered |> text]
        _ ->
            span [] []

renderRelease : Model -> Release -> Html Msg
renderRelease model release =
    div []
        [ div [ class "releaseHeader" ]
            [ release.name ++ " (" ++ (toString release.year) ++ ")" |> text ]
        , div [ class "releaseInfo" ]
              [ div []
                    [ img [ class "releaseLarge", src ("/" ++ release.imgsrc) ] [] ]
              ]
        , maybeRenderStory release
        , div [ class "releaseInfo" ]
              [ maybeTracklist release
              , maybeLabel release
              , maybeFormat release
              , maybeRecorded release
              , maybeMastered release
              , otherVersions model release
              ]
        ]

release : Model -> Html Msg
release model =
    case model.release of
        RemoteData.NotAsked ->
            div [] [ text "Getting release..."]
        RemoteData.Loading ->
            div [] [ text "Getting release..."]
        RemoteData.Failure _ ->
            error
        RemoteData.Success release ->
            renderRelease model release

---- ABOUT ----

about : Html Msg
about =
    div [] [ p [] [ text "Coke Bust is a Washington, D.C. straight edge hardcore band that was formed in 2006. "]
            , p [] [ text "The band has since released two LP's and various EP's and splits. "]
            , p [] [ text "Coke Bust has toured North and South America, Europe, and Japan. "]
            , p [] [ text "Thanks for visiting our website!" ]
            ]
---- NOT FOUND ----

notFound : Html Msg
notFound =
    div [] [ text "error 404 not found bro" ]

---- SPLASH ----

splash : Html Msg
splash =
    div [ class "splash" ]
        [ navTo "/news" []
            [ img [ src "/billysfriend.jpg", height 500 ] []
            , br [] []
            , p [ style [ ("color", "black") ]] [ text "[click!]"]
            ]
        , p [] [ text "This site is not GDPR compliant. Sod off."]
        ]

---- HOME & NAV STUFF ----

home : Model -> Html Msg
home model =
    div [ class "home" ]
        [ nav model
        , footer
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
          [ img [class "jubertFooter", src "/jubertleft.jpg" ] [] ]
    , div [ class "footSubDiv" ]
        [ footerLink "https://cokebust.bandcamp.com/" "bandcamp", footerSpacer
        , footerLink "https://twitter.com/cokebust?lang=en" "twitter", footerSpacer
        , footerLink "https://www.facebook.com/cokebust/" "facebook", footerSpacer
        , footerLink "mailto:cokebust@gmail.com" "cokebust@gmail.com"
        ]
    , div [ class "footSubDiv" ]
          [ img [ class "jubertFooter", src "/jubertright.jpg" ] [] ]
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
            [ navTo url [] [ text title ] ]
