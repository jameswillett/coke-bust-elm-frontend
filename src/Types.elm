module Types exposing (..)

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

type alias Show =
    { id : Int
    , date : String
    , venue : String
    , address : String
    , time : String
    , notes : String
    , links : String
    }
