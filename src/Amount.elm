module Amount exposing
    ( Amount
    , add
    , fromInt
    , saldo
    , show
    , subtract
    , toCurrencyString
    , toInt
    )

import Money


type Amount
    = Dr Int
    | Cr Int


toInt : Amount -> Int
toInt amount =
    case amount of
        Dr i ->
            -i

        Cr i ->
            i


fromInt : Int -> Amount
fromInt amount =
    if amount < 0 then
        Dr (-1 * amount)

    else
        Cr amount


add : Amount -> Amount -> Amount
add a b =
    fromInt (toInt a + toInt b)


subtract : Amount -> Amount -> Amount
subtract a b =
    fromInt (toInt a - toInt b)


toCurrencyString : Money.Code -> Amount -> String
toCurrencyString code amount =
    toInt amount |> formatAsCurrency code


show : Amount -> String
show =
    String.fromInt << toInt


saldo : List { a | amount : Amount } -> Amount
saldo entries =
    let
        extractAndAddAmounts entry amount =
            add amount entry.amount
    in
    List.foldl extractAndAddAmounts (Cr 0) entries


formatAsCurrency : Money.Code -> Int -> String
formatAsCurrency code representation =
    let
        moneyString =
            String.fromInt representation

        currency =
            Money.currencyFromCode code

        fraction =
            moneyString
                |> String.right currency.decimalDigits
                |> String.padLeft currency.decimalDigits '0'

        isNegative =
            String.startsWith "-" moneyString

        wholeUnits =
            if isNegative then
                moneyString
                    |> String.dropRight currency.decimalDigits
                    |> String.dropLeft 1
                    |> String.padLeft 1 '0'

            else
                moneyString
                    |> String.dropRight currency.decimalDigits
                    |> String.padLeft 1 '0'

        sign =
            if isNegative then
                "-"

            else
                ""
    in
    sign
        ++ wholeUnits
        ++ (if fraction == "" then
                ""

            else
                "," ++ fraction
           )
        ++ " "
        ++ currency.symbol
