module Main exposing (..)

import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (src, class, classList)
import WebSocket
import Dict exposing (Dict)
import Json.Decode exposing (..)


---- MODEL ----


subscribeInstrumentNames : List String
subscribeInstrumentNames =
    [ "EURUSD", "BTCUSD", "EURGBP", "USDJPY", "GBPUSD", "USDCHF", "USDCAD", "AUDUSD", "EURJPY", "EURCHF" ]


type Grow
    = UP
    | DOWN
    | STAY


isGrow : Float -> Float -> Grow
isGrow old new =
    if old == new then
        STAY
    else if old > new then
        DOWN
    else
        UP


type alias Quote =
    { s : String, b : String, a : String, spr : String }


type alias Instrument =
    { name : String
    , bid : Float
    , bidGrow : Grow
    , ask : Float
    , askGrow : Grow
    , spread : String
    }


subscribeString : List String -> String
subscribeString instruments =
    "SUBSCRIBE: " ++ (String.join "," instruments)


type alias Model =
    { instruments : Dict String Instrument
    , message : String
    }


quotesServer : String
quotesServer =
    "wss://quotes.exness.com:18400/"


initialCmd : Cmd Msg
initialCmd =
    subscribeString subscribeInstrumentNames
        |> WebSocket.send quotesServer


init : ( Model, Cmd Msg )
init =
    ( { instruments = Dict.empty
      , message = ""
      }
    , initialCmd
    )



---- UPDATE ----


instrumentDecoder : Decoder Quote
instrumentDecoder =
    map4 Quote (field "s" string) (field "b" string) (field "a" string) (field "spr" string)


responseDecoder : Decoder (List Quote)
responseDecoder =
    field "ticks" (list instrumentDecoder)


type Msg
    = Echo String


instrumentFromQuote : Quote -> Instrument
instrumentFromQuote quote =
    let
        bid =
            Result.withDefault 0 (String.toFloat quote.b)

        ask =
            Result.withDefault 0 (String.toFloat quote.a)
    in
        Instrument quote.s bid STAY ask STAY quote.spr


newInstrumentsFromQuotes : Dict String Instrument -> List Quote -> Dict String Instrument
newInstrumentsFromQuotes oldInstruments quotes =
    quotes
        |> List.map
            (\quote ->
                case Dict.get quote.s oldInstruments of
                    Nothing ->
                        instrumentFromQuote quote

                    Just oldInstrument ->
                        let
                            newInstrument =
                                instrumentFromQuote quote

                            bidGrow =
                                isGrow oldInstrument.bid newInstrument.bid

                            askGrow =
                                isGrow oldInstrument.ask newInstrument.ask
                        in
                            ({ newInstrument | askGrow = askGrow, bidGrow = bidGrow })
            )
        |> List.map (\x -> ( x.name, x ))
        |> Dict.fromList


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Echo response ->
            let
                newInstruments =
                    case decodeString responseDecoder response of
                        Err msg ->
                            model.instruments

                        Ok quotes ->
                            newInstrumentsFromQuotes model.instruments quotes
            in
                ( { model | instruments = Dict.union newInstruments model.instruments }, Cmd.none )



---- VIEW ----


viewGrow : Grow -> Html Msg
viewGrow grow =
    div [ class "instrument__grow", classList [ ( "instrument__grow--up", grow == UP ), ( "instrument__grow--down", grow == DOWN ) ] ]
        []


viewInstrument : Instrument -> Html Msg
viewInstrument instrument =
    div [ class "instrument" ]
        [ div [ class "instrument__top" ]
            [ div [ class "instrument__name" ] [ text instrument.name ]
            , div [ class "instrument__spread" ] [ text ("Sprd " ++ instrument.spread) ]
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
            (subscribeInstrumentNames
                |> List.map (\x -> Dict.get x model.instruments)
                |> List.filterMap identity
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
