module View.Day exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.Events exposing (..)
import Util.Time exposing (Date)


{-| Don't modify
-}
type alias DailyData =
    { date : Date
    , highTemp : Maybe Float
    , lowTemp : Maybe Float
    , totalPrecipitaion : Float
    }


{-| Generates Html based on `DailyData`

Some relevant functions:

  - `Util.Time.formatDate`

-}
view : DailyData -> Html msg
view dailyData =
    let
        checkIfNothing: Maybe Float -> String
        checkIfNothing something =
            case something of
                Just smth -> String.fromFloat smth
                Nothing -> "unavailable"
    in
        div [class "day"] 
        [
            p [class "day-date"] [text (Util.Time.formatDate dailyData.date)]
        ,   p [class "day-precipitation"] [text (String.fromFloat dailyData.totalPrecipitaion)]  
        ,   p [class "day-hightemp"] [text (checkIfNothing dailyData.highTemp)]
        ,   p [class "day-lowtemp"] [text (checkIfNothing dailyData.lowTemp)]
        ]