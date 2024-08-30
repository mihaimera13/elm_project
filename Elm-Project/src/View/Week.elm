module View.Week exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, style)
import Html.Events exposing (..)
import Util.Time exposing (Date, formatDate)
import View.Day exposing (DailyData)


type alias WeeklyData =
    { dailyData : List DailyData
    }


{-| Generates Html based on `WeeklyData`

Some relevant functions:

  - `Util.Time.formatDate`

-}
view : WeeklyData -> Html msg
view weeklyData =
    let 
        dailyDataList: WeeklyData -> List (Html msg)
        dailyDataList wd =
                case weeklyData.dailyData of
                    [] -> [text "No data"]
                    x::xs -> List.map View.Day.view wd.dailyData
    in
        div [class "week"] 
        [
            h2 [] (dailyDataList weeklyData)
        ]
        
        
    
