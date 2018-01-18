module Main exposing (..)

import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (src, class, classList)
import WebSocket
import Dict exposing (Dict)
import Json.Decode exposing (..)


---- MODEL ----


type Grow
    = UP
    | DOWN
    | STAY


isGrow old new =
    if old == new then
        STAY
    else if old > new then
        DOWN
    else
        UP


type alias InstrumentFromSocket =
    { s : String, b : String, a : String, spr : String }


type alias Instrument =
    { name : String
    , bid : Float
    , bidGrow : Grow
    , ask : Float
    , askGrow : Grow
    }


subscribeInsrtrumentNames =
    [ "EURUSD", "BTCUSD" ]


subscribeString instruments =
    "SUBSCRIBE: " ++ (String.join "," instruments)


type alias Model =
    { instruments : Dict String Instrument
    , message : String
    }


quotesServer : String
quotesServer =
    "wss://quotes.exness.com:18400/"


initialCmd =
    subscribeString subscribeInsrtrumentNames
        |> WebSocket.send quotesServer


init : ( Model, Cmd Msg )
init =
    ( { instruments = Dict.empty
      , message = ""
      }
    , initialCmd
    )



---- UPDATE ----


instrumentDecoder =
    map4 InstrumentFromSocket (field "s" string) (field "b" string) (field "a" string) (field "spr" string)


responseDecoder =
    field "ticks" (list instrumentDecoder)


type Msg
    = Echo String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Echo response ->
            let
                newInstruments =
                    case decodeString responseDecoder response of
                        Err msg ->
                            model.instruments

                        Ok instruments ->
                            instruments
                                |> List.map
                                    (\instrument ->
                                        case Dict.get instrument.s model.instruments of
                                            Nothing ->
                                                ( instrument.s, (Instrument instrument.s 0 STAY 0 STAY) )

                                            Just oldInstrument ->
                                                let
                                                    newBid =
                                                        Result.withDefault 0 (String.toFloat instrument.b)

                                                    newAsk =
                                                        Result.withDefault 0 (String.toFloat instrument.a)

                                                    bidGrow =
                                                        isGrow oldInstrument.bid newBid

                                                    askGrow =
                                                        isGrow oldInstrument.ask newAsk
                                                in
                                                    ( instrument.s, (Instrument instrument.s newBid bidGrow newAsk askGrow) )
                                    )
                                |> Dict.fromList
            in
                ( { model | instruments = Dict.union newInstruments model.instruments }, Cmd.none )



---- VIEW ----


viewGrow grow =
    div [ class "instrument__grow", classList [ ( "instrument__grow--up", grow == UP ), ( "instrument__grow--down", grow == DOWN ) ] ]
        []


viewInstrument : Instrument -> Html Msg
viewInstrument instrument =
    div [ class "instrument" ]
        [ div [ class "instrument__top" ]
            [ div [ class "instrument__name" ] [ text instrument.name ]
            ]
        , div [ class "instrument__values" ]
            [ viewGrow instrument.askGrow
            , div [ class "instrument__text" ]
                [ text <| toString <| instrument.bid
                , text " / "
                , text <| toString <| instrument.ask
                ]
            , viewGrow instrument.bidGrow
            ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ div [ class "instrument__list" ]
            (model.instruments
                |> Dict.toList
                |> List.map Tuple.second
                |> List.map viewInstrument
            )
        ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen quotesServer Echo



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
