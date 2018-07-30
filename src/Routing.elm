module Routing exposing (..)
import UrlParser exposing (..)
import Navigation
import Types exposing (..)

matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ map Root top
        , map NewsRoute (s "news")
        , map NewsArchive (s "news" </> int)
        , map ShowsRoute (s "shows")
        , map ShowArchiveRoute (s "shows" </> s "all")
        , map StoreRoute (s "store")
        , map DiscographyRoute (s "releases")
        , map ReleaseRoute (s "releases" </> int)
        , map AboutRoute (s "about")
        ]

parseLocation : Navigation.Location -> Route
parseLocation location =
    case (parsePath matchers location) of
        Just route ->
            route
        Nothing ->
            NotFound
