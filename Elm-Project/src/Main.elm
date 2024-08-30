module Main exposing (..)

import Browser
import Chart.Item as CI
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import Http
import Model exposing (Config, Mode(..), Model, Weather)
import Model.WeatherData as WeatherData exposing (ApiWeatherData, HourlyDataPoint)
import Model.WeatherItems exposing (SelectedWeatherItems, WeatherItem(..))
import Task
import Time
import Url.Builder as UrlBuilder
import Util
import Util.Time
import View.WeatherChart exposing (showAllItems)
import View.WeatherItems
import View.Week


{-| Don't modify
-}
type Msg
    = GotTime Time.Posix
    | GetWeather
    | GotWeather (Result Http.Error ApiWeatherData)
    | OnHover (List (CI.One HourlyDataPoint CI.Dot))
    | ChangeWeatherItemSelection WeatherItem Bool


prodFlags : Config
prodFlags =
    { apiUrl = "https://api.open-meteo.com", mode = Prod }


devFlags : Config
devFlags =
    { apiUrl = "http://localhost:3000", mode = Dev }


{-| Create a program that uses the "production" configuration (uses the real API to get the weather data)
-}
main : Program () Model Msg
main =
    Browser.element
        { init = init prodFlags
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


{-| Create a program that uses the development configuration (uses the local server to get the weather data)
-}
reactorMain : Program () Model Msg
reactorMain =
    Browser.element
        { init = init devFlags
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


{-| Don't modify
-}
init : Config -> () -> ( Model, Cmd Msg )
init flags _ =
    ( Model.initModel flags
    , Task.perform GotTime Time.now
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


getWeather : String -> Cmd Msg
getWeather apiUrl =
    let
        queryParams =
            List.concat
                [ [ UrlBuilder.string "latitude" <| String.fromFloat 46.77
                  , UrlBuilder.string "longitude" <| String.fromFloat 23.6
                  , UrlBuilder.string "timezone" "auto"
                  , UrlBuilder.string "timeformat" "unixtime"
                  ]
                , List.map (UrlBuilder.string "hourly")
                    [ "temperature_2m"
                    , "precipitation"
                    ]
                ]
    in
    Http.get
        { url = UrlBuilder.crossOrigin apiUrl [ "v1", "forecast" ] queryParams
        , expect = Http.expectJson GotWeather WeatherData.decodeWeatherData
        }


adjustTime : Int -> Time.Posix -> Time.Posix
adjustTime offset time =
    time |> Time.posixToMillis |> (\m -> m + 1000 * offset) |> Time.millisToPosix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        ( newState, cmd ) =
            case ( msg, model.state ) of
                ( GotTime time, _ ) ->
                    ( Model.HaveTime { time = time }
                    , getWeather model.config.apiUrl
                    )

                ( GotWeather res, Model.HaveTime state ) ->
                    case res of
                        Ok weather ->
                            ( Model.HaveWeatherAndTime
                                { time = adjustTime weather.utcOffset state.time
                                , weather = weather
                                , hovering = []
                                , selectedItems = Model.WeatherItems.allSelected
                                }
                            , Cmd.none
                            )

                        Err err ->
                            ( Model.FailedToLoad, Cmd.none )

                ( OnHover hovering, Model.HaveWeatherAndTime data ) ->
                    ( Model.HaveWeatherAndTime { data | hovering = hovering }, Cmd.none )

                ( ChangeWeatherItemSelection item newValue, Model.HaveWeatherAndTime data ) ->
                     let 
                        selected = data.selectedItems |> Model.WeatherItems.set item newValue
                     in
                        (Model.HaveWeatherAndTime { data | selectedItems = selected }, Cmd.none)

                _ ->
                    ( model.state, Cmd.none )
    in
    ( { model | state = newState }, cmd )


{-| Derive (extract) the data required for the weather chart from the state of the app

Some relevant functions:

  - `WeatherData.toHourlyDataPoints`
  - `Util.minimumBy`, `Util.maximumBy`

-}
getChartData : Weather -> View.WeatherChart.ChartData
getChartData wthr =
    let
        time = wthr.time
        weather = wthr.weather
        hovering = wthr.hovering
        hourlyPoints = WeatherData.toHourlyDataPoints weather
        minPoint = Util.minimumBy .temperature hourlyPoints
        maxPoint = Util.maximumBy .temperature hourlyPoints
        itemsToShow = wthr.selectedItems
        
    in
        View.WeatherChart.ChartData
            time
            minPoint
            maxPoint
            hourlyPoints
            hovering
            itemsToShow
            

{-| Derive (extract) the data required for the weather chart from the state of the app

Some relevant functions:

  - `WeatherData.toHourlyDataPoints`
  - `List.minimum`, `List.maximum`
  - `Util.Time.posixToDate`

-}
getWeeklyData : Weather -> View.Week.WeeklyData
getWeeklyData wthr =
    let
        weather = wthr.weather
        offset = weather.utcOffset
        hourlyPoints = WeatherData.toHourlyDataPoints weather
        
        timeToUtc t off =
           t |> Time.posixToMillis |> (\m -> m - 1000 * off) |> Time.millisToPosix
           
        dailyPoints = Util.groupBy (\x -> (Util.Time.posixToDate Time.utc x.time)) hourlyPoints
        
        toDailydata dp acc =
            case dp of
            [] -> List.reverse acc
            x::xs -> 
                let
                    extractTemp: {time:Time.Posix,temperature:Float,precipitation:Float} -> Float
                    extractTemp {temperature} = temperature
                    extractPrec: {time:Time.Posix,temperature:Float,precipitation:Float} -> Float
                    extractPrec {precipitation} = precipitation
                    date = (\(u,v) -> u) x
                    tempList = List.map (\x1 -> extractTemp x1) ((\(u,v)-> v) x)
                    precList = List.map (\x1 -> extractPrec x1) ((\(u,v)-> v) x)
                    highTemp = List.maximum tempList
                    lowTemp = List.minimum tempList
                    totalPrecipitaion = List.foldl (+) 0 precList
                in
                    toDailydata xs ({date=date,highTemp=highTemp,lowTemp=lowTemp,totalPrecipitaion=totalPrecipitaion}::acc)
                    
    in
        View.Week.WeeklyData (toDailydata dailyPoints [])

view : Model -> Html Msg
view model =
    case model.state of
        Model.FailedToLoad ->
            -- div [] []
            div [] [ text "Failed to load" ]

        Model.WaitingForTime ->
            -- div [] []
            div [] [ text "Obtaining the current time" ]

        Model.HaveTime _ ->
            -- div [] []
            div [] [ text "Loading the weather" ]

        Model.HaveWeatherAndTime data ->
            div []
                [ h1 []
                    [ h1 []
                        [ if model.config.mode == Model.Dev then
                            text "Weather (DEV)"

                          else
                            text "Weather"
                        ]
                    ]
                , View.WeatherItems.view { onChangeSelection = ChangeWeatherItemSelection} data.selectedItems

                -- Comment to make the code compile
                , View.WeatherChart.view { onHover = OnHover } (getChartData data) -- Comment to prevent crash if getChartData is not implemented
                , View.Week.view (getWeeklyData data) -- Comment to prevent crash if getWeeklyData is not implemented
                ]
