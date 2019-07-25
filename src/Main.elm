module Main exposing (main)

import Amount exposing (Amount)
import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Form as Form
import Bootstrap.Form.Fieldset as Fieldset
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Table as Table
import Browser
import Html exposing (Html, button, div, label, p, text)
import Html.Attributes exposing (class, for, id, value)
import Html.Events exposing (onClick, onInput)
import Http
import Iso8601
import Json.Decode exposing (Decoder, field, float, map2)
import Money
import Round
import Time exposing (Month(..), Posix, toDay, toMonth, toYear, utc)


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- Model


type alias Model =
    { inputAmountField : String
    , inputDateField : String
    , foreignAccount : List LedgerEntry
    , ledgerEntries : List LedgerEntry
    }


type alias LedgerEntry =
    { amount : Amount
    , rate : Float
    , date : Posix
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { inputAmountField = ""
      , inputDateField = ""
      , foreignAccount = []
      , ledgerEntries = []
      }
    , Cmd.none
    )



-- Update


type Msg
    = SyncForeignAccount RateData
    | AddUSD Int String
    | SyncAddUSD ( RateData, Int )
    | UpdateAmountInput String
    | UpdateDateInput String
    | NoOp


type alias RateData =
    { date : Posix
    , eurRate : Float
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SyncForeignAccount rateData ->
            ( syncForeignAcc rateData model, Cmd.none )

        AddUSD value date ->
            ( { model
                | inputAmountField = ""
                , inputDateField = ""
              }
            , syncAddUSD value date
            )

        SyncAddUSD ( rateData, value ) ->
            ( model
                |> syncForeignAcc rateData
                |> addUsdEntry value rateData
                |> addLedgerEntry value rateData
            , Cmd.none
            )

        UpdateAmountInput newValue ->
            ( { model | inputAmountField = newValue }, Cmd.none )

        UpdateDateInput newValue ->
            ( { model | inputDateField = newValue }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


syncAddUSD : Int -> String -> Cmd Msg
syncAddUSD value date =
    Http.get
        { url = ratesUrlFromDate date
        , expect = Http.expectJson (pairValueWithResult value) rateDataDecoder
        }


ratesUrlFromDate : String -> String
ratesUrlFromDate dateString =
    "https://api.exchangeratesapi.io/" ++ dateString ++ "?base=USD&symbols=EUR"


pairValueWithResult : Int -> Result Http.Error RateData -> Msg
pairValueWithResult value result =
    case result of
        Ok rateData ->
            SyncAddUSD ( rateData, value )

        Err _ ->
            NoOp


rateDataDecoder : Decoder RateData
rateDataDecoder =
    map2 RateData
        (field "date" Iso8601.decoder)
        (field "rates" (field "EUR" float))


addUsdEntry : Int -> RateData -> Model -> Model
addUsdEntry usd rateData model =
    let
        newEntry =
            { amount = Amount.fromInt usd
            , rate = rateData.eurRate
            , date = rateData.date
            }
    in
    { model | foreignAccount = newEntry :: model.foreignAccount }


addLedgerEntry : Int -> RateData -> Model -> Model
addLedgerEntry usd rateData model =
    let
        newEntry =
            { amount =
                toFloat usd
                    * rateData.eurRate
                    -- use "half away from zero" rounding
                    |> Round.roundNumCom 0
                    |> round
                    |> Amount.fromInt
            , rate = rateData.eurRate
            , date = rateData.date
            }
    in
    { model | ledgerEntries = newEntry :: model.ledgerEntries }


syncForeignAcc : RateData -> Model -> Model
syncForeignAcc rateData model =
    let
        { foreignAccount, ledgerEntries } =
            model
    in
    if List.length foreignAccount == 0 then
        model

    else
        let
            oldLocalSaldo =
                Amount.saldo ledgerEntries

            newLocalSaldo =
                Amount.saldo foreignAccount
                    |> Amount.toInt
                    |> toFloat
                    |> (\f -> rateData.eurRate * f)
                    -- Use "half away from zero" rounding
                    |> Round.roundNumCom 0
                    |> round
                    |> Amount.fromInt

            syncEntry =
                { amount = Amount.subtract newLocalSaldo oldLocalSaldo, rate = rateData.eurRate, date = rateData.date }
        in
        { model | ledgerEntries = syncEntry :: model.ledgerEntries }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- View


view : Model -> Html Msg
view model =
    Grid.container []
        [ CDN.stylesheet
        , viewTable model
        , viewInputs model
        , p [ class "text-center h1" ]
            [ text (Amount.toCurrencyString Money.USD (Amount.saldo model.foreignAccount)) ]
        ]


viewTable : Model -> Html Msg
viewTable model =
    Table.table
        { options = [ Table.striped ]
        , thead =
            Table.simpleThead
                [ Table.th [] [ text "Buchungsdatum" ]
                , Table.th [] [ text "Betrag in Euro" ]
                , Table.th [] [ text "Wechselkurs USD -> EUR" ]
                ]
        , tbody =
            Table.tbody []
                (model.ledgerEntries |> List.reverse |> List.map viewEntry)
        }


viewInputs : Model -> Html Msg
viewInputs model =
    div []
        [ Fieldset.config
            |> Fieldset.asGroup
            |> Fieldset.children
                [ Grid.container []
                    [ Grid.simpleRow
                        [ Grid.col
                            [ Col.xs12, Col.md3 ]
                            [ Form.label [ for "amountField" ]
                                [ text "Betrag"
                                , Input.number
                                    [ Input.onInput UpdateAmountInput
                                    , Input.id "amountField"
                                    , Input.value model.inputAmountField
                                    ]
                                ]
                            ]
                        , Grid.col
                            [ Col.xs12, Col.md3 ]
                            [ Form.label [ for "dateField" ]
                                [ text "Datum"
                                , Input.date
                                    [ Input.onInput UpdateDateInput
                                    , Input.id "dateField"
                                    , Input.value model.inputDateField
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            |> Fieldset.view
        , Grid.container []
            [ Button.button [ Button.primary, Button.onClick (submitInputField model.inputAmountField model.inputDateField) ]
                [ text "Submit"
                ]
            ]
        ]


submitInputField : String -> String -> Msg
submitInputField value date =
    let
        maybeInt =
            String.toInt value
    in
    case maybeInt of
        Just int ->
            AddUSD int date

        Nothing ->
            NoOp


viewEntry : LedgerEntry -> Table.Row Msg
viewEntry entry =
    Table.tr []
        [ Table.td []
            [ text
                (String.join "."
                    [ String.fromInt (toDay utc entry.date)
                    , posixToMonthNumberString (toMonth utc entry.date)
                    , String.fromInt (toYear utc entry.date)
                    ]
                )
            ]
        , Table.td []
            [ text (Amount.toCurrencyString Money.EUR entry.amount)
            ]
        , Table.td []
            [ text (Round.round 5 entry.rate) ]
        ]


posixToMonthNumberString : Time.Month -> String
posixToMonthNumberString month =
    case month of
        Jan ->
            "01"

        Feb ->
            "02"

        Mar ->
            "03"

        Apr ->
            "04"

        May ->
            "05"

        Jun ->
            "06"

        Jul ->
            "07"

        Aug ->
            "08"

        Sep ->
            "09"

        Oct ->
            "10"

        Nov ->
            "11"

        Dec ->
            "12"
