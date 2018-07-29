module Types exposing (..)

import RemoteData exposing (WebData)
import Navigation
import Http

import Routing exposing (..)


type Msg
    = NoOp
    | UrlChange Navigation.Location
    | ChangeLocation String
    | ShowResponse (WebData (List Show))

type alias Model =
    { path : String
    , route : Route
    , config : Config
    , shows : WebData (List Show)
    , news : WebData (List NewsEntry)
    , releases : WebData (List Release)
    }

type alias AllDataResult =
    { shows : List Show
    , news : List NewsEntry
    , releases : List Release
    }

type alias Config =
    { api_url : String
    }

type alias Release =
    { id : Int
    , name : String
    , year : Int
    , label : String
    , format : Maybe String
    , recorded : Maybe String
    , mastered : Maybe String
    , story : Maybe String
    , tracklist : String
    , imgsrc : String
    , meta : String
}

type alias NewsEntry =
    { author : String
    , content : String
    , date : String
    , id : Int
    }

type alias Shows =
    { shows : List Show }

type alias Show =
    { id : Int
    , date : String
    , venue : String
    , address : String
    , time : Maybe String
    , notes : Maybe String
    , links : Maybe String
    }

type alias AllData =
    { shows : List Show
    , news : List NewsEntry
    , releases : List Release
    }
