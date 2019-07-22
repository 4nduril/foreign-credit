module Main exposing (main)

import Browser
import Html exposing (Html, button, div, fieldset, input, label, li, text, ul)
import Html.Attributes exposing (for, id, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (Decoder, field, float, map2, string)


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
    }


type Amount
    = Dr Int
    | Cr Int


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
    { date : String
    , eurRate : Float
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SyncForeignAccount rateData ->
            ( syncForeignAcc rateData model, Cmd.none )

        AddUSD value date ->
            ( { model | inputAmountField = "", inputDateField = "" }, syncAddUSD value date )

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
    map2 RateData (field "date" string) (field "rates" (field "EUR" float))


addUsdEntry : Int -> RateData -> Model -> Model
addUsdEntry usd rateData model =
    let
        newEntry =
            { amount = toAmount usd, rate = rateData.eurRate }
    in
    { model | foreignAccount = newEntry :: model.foreignAccount }


addLedgerEntry : Int -> RateData -> Model -> Model
addLedgerEntry usd rateData model =
    let
        newEntry =
            { amount = toFloat usd * rateData.eurRate |> round |> toAmount, rate = rateData.eurRate }
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
                saldo ledgerEntries

            newLocalSaldo =
                saldo foreignAccount
                    |> fromAmount
                    |> toFloat
                    |> (\f -> rateData.eurRate * f)
                    |> round
                    |> toAmount

            syncEntry =
                { amount = subtractAmounts newLocalSaldo oldLocalSaldo, rate = rateData.eurRate }
        in
        { model | ledgerEntries = syncEntry :: model.ledgerEntries }


fromAmount : Amount -> Int
fromAmount amount =
    case amount of
        Dr i ->
            -i

        Cr i ->
            i


toAmount : Int -> Amount
toAmount amount =
    if amount < 0 then
        Dr (-1 * amount)

    else
        Cr amount


amountToString : Amount -> String
amountToString amount =
    case amount of
        Cr int ->
            String.fromInt int

        Dr int ->
            String.fromInt int


saldo : List { a | amount : Amount } -> Amount
saldo entries =
    let
        extractAndAddAmounts entry amount =
            addAmounts amount entry.amount
    in
    List.foldl extractAndAddAmounts (Cr 0) entries


addAmounts : Amount -> Amount -> Amount
addAmounts a b =
    toAmount (fromAmount a + fromAmount b)


subtractAmounts : Amount -> Amount -> Amount
subtractAmounts a b =
    toAmount (fromAmount a - fromAmount b)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- View


view : Model -> Html Msg
view model =
    div []
        [ ul []
            (model.ledgerEntries |> List.reverse |> List.map viewEntry)
        , fieldset []
            [ label [ for "amountField" ]
                [ text "Betrag"
                , input
                    [ onInput UpdateAmountInput
                    , id "amountField"
                    , type_ "number"
                    , value model.inputAmountField
                    ]
                    []
                ]
            , label [ for "dateField" ]
                [ text "Datum"
                , input
                    [ onInput UpdateDateInput
                    , id "dateField"
                    , type_ "date"
                    , value model.inputDateField
                    ]
                    []
                ]
            ]
        , div []
            [ button [ onClick (submitInputField model.inputAmountField model.inputDateField) ]
                [ text "Submit"
                ]
            ]
        , div []
            [ text "USD: ", text (amountToString (saldo model.foreignAccount)) ]
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


viewEntry : LedgerEntry -> Html Msg
viewEntry entry =
    li []
        [ text (amountToString entry.amount)
        ]
