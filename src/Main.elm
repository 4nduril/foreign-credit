module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Task exposing (Task, andThen, succeed)
import Time


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- Model


type alias Model =
    { inputField : String
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
    ( { inputField = ""
      , foreignAccount = []
      , ledgerEntries = []
      }
    , Cmd.none
    )



-- Update


type Msg
    = SyncForeignAccount Float
    | AddUSD Int
    | SyncAddUSD ( Float, Int )
    | UpdateInput String
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SyncForeignAccount rate ->
            ( syncForeignAcc rate model, Cmd.none )

        AddUSD value ->
            ( { model | inputField = "" }, syncAddUSD value )

        SyncAddUSD ( rate, value ) ->
            ( model
                |> syncForeignAcc rate
                |> addUsdEntry value rate
                |> addLedgerEntry value rate
            , Cmd.none
            )

        UpdateInput newValue ->
            ( { model | inputField = newValue }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


syncAddUSD : Int -> Cmd Msg
syncAddUSD value =
    Task.perform SyncAddUSD (succeed 0.8 |> Task.map (\rate -> ( rate, value )))


addUsdEntry : Int -> Float -> Model -> Model
addUsdEntry usd rate model =
    let
        newEntry =
            { amount = toAmount usd, rate = rate }
    in
    { model | foreignAccount = newEntry :: model.foreignAccount }


addLedgerEntry : Int -> Float -> Model -> Model
addLedgerEntry usd rate model =
    let
        newEntry =
            { amount = toFloat usd * rate |> round |> toAmount, rate = rate }
    in
    { model | ledgerEntries = newEntry :: model.ledgerEntries }


syncForeignAcc : Float -> Model -> Model
syncForeignAcc rate model =
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
                    |> (\f -> rate * f)
                    |> round
                    |> toAmount

            syncEntry =
                { amount = subtractAmounts newLocalSaldo oldLocalSaldo, rate = rate }
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
subscriptions model =
    Sub.none



-- View


view : Model -> Html Msg
view model =
    div []
        [ ul []
            (model.ledgerEntries |> List.reverse |> List.map viewEntry)
        , input
            [ onInput UpdateInput
            , type_ "number"
            , value model.inputField
            ]
            []
        , button [ onClick (submitInputField model.inputField) ]
            [ text "Submit"
            ]
        , div []
            [ text "USD: ", text (amountToString (saldo model.foreignAccount)) ]
        ]


submitInputField : String -> Msg
submitInputField value =
    let
        maybeInt =
            String.toInt value
    in
    case maybeInt of
        Just int ->
            AddUSD int

        Nothing ->
            NoOp


viewEntry : LedgerEntry -> Html Msg
viewEntry entry =
    li []
        [ text (amountToString entry.amount)
        ]
