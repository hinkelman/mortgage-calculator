module Main exposing (main)

import Browser
import Html exposing (Html, div, footer, h1, h2, header, input, label, li, main_, p, section, span, strong, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (attribute, class, style, type_, value)
import Html.Events exposing (onInput)
import Mortgage exposing (Summary)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type Buyer
    = A
    | B


type Field
    = Price
    | DownA
    | DownB
    | Rate
    | Years
    | TaxRate
    | Insurance
    | Hoa
    | PmiRate
    | InternalRate
    | SellYears
    | Appreciation
    | SellingCosts


type alias Model =
    { nameA : String
    , nameB : String
    , price : String
    , downA : String
    , downB : String
    , rate : String
    , years : String
    , taxRate : String
    , insurance : String
    , hoa : String
    , pmiRate : String
    , internalRate : String
    , sellYears : String
    , appreciation : String
    , sellingCosts : String
    }


init : Model
init =
    { nameA = "Alex"
    , nameB = "Jordan"
    , price = "700,000"
    , downA = "140,000"
    , downB = "0"
    , rate = "6.5"
    , years = "30"
    , taxRate = "1.1"
    , insurance = "1,800"
    , hoa = "0"
    , pmiRate = "0.5"
    , internalRate = "0"
    , sellYears = "10"
    , appreciation = "3"
    , sellingCosts = "6"
    }



-- UPDATE


type Msg
    = SetField Field String
    | SetName Buyer String


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetField field s ->
            case field of
                Price ->
                    { model | price = s }

                DownA ->
                    { model | downA = s }

                DownB ->
                    { model | downB = s }

                Rate ->
                    { model | rate = s }

                Years ->
                    { model | years = s }

                TaxRate ->
                    { model | taxRate = s }

                Insurance ->
                    { model | insurance = s }

                Hoa ->
                    { model | hoa = s }

                PmiRate ->
                    { model | pmiRate = s }

                InternalRate ->
                    { model | internalRate = s }

                SellYears ->
                    { model | sellYears = s }

                Appreciation ->
                    { model | appreciation = s }

                SellingCosts ->
                    { model | sellingCosts = s }

        SetName A s ->
            { model | nameA = s }

        SetName B s ->
            { model | nameB = s }



-- PARSING


parseNumber : String -> Maybe Float
parseNumber s =
    s
        |> String.filter (\c -> c /= ',' && c /= '$' && c /= '%' && c /= ' ')
        |> String.toFloat


type alias Parsed =
    { inputs : Mortgage.Inputs
    , sale : Mortgage.SaleInputs
    }


parse : Model -> Result (List String) Parsed
parse model =
    let
        fields =
            [ ( "Home price", model.price )
            , ( nameOf model A ++ "'s down payment", model.downA )
            , ( nameOf model B ++ "'s down payment", model.downB )
            , ( "Interest rate", model.rate )
            , ( "Loan term", model.years )
            , ( "Property tax rate", model.taxRate )
            , ( "Home insurance", model.insurance )
            , ( "HOA dues", model.hoa )
            , ( "PMI rate", model.pmiRate )
            , ( "Interest on the down payment difference", model.internalRate )
            , ( "Years until sale", model.sellYears )
            , ( "Selling costs", model.sellingCosts )
            ]

        values =
            List.map (\( lbl, s ) -> ( lbl, parseNumber s )) fields

        unparseable =
            List.filterMap
                (\( lbl, v ) ->
                    case v of
                        Nothing ->
                            Just (lbl ++ " must be a number.")

                        Just _ ->
                            Nothing
                )
                values

        get s =
            parseNumber s |> Maybe.withDefault 0

        inputs =
            { price = get model.price
            , downA = get model.downA
            , downB = get model.downB
            , annualRate = get model.rate
            , years = round (get model.years)
            , taxRate = get model.taxRate
            , insurance = get model.insurance
            , hoa = get model.hoa
            , pmiRate = get model.pmiRate
            , internalRate = get model.internalRate
            }

        sale =
            { years = round (get model.sellYears)
            , appreciation = get model.appreciation
            , sellingCosts = get model.sellingCosts
            }

        -- Appreciation is checked on its own because it may be negative.
        appreciationError =
            case parseNumber model.appreciation of
                Nothing ->
                    [ "Appreciation must be a number." ]

                Just g ->
                    if g <= -100 then
                        [ "Appreciation must be greater than −100%." ]

                    else
                        []

        negatives =
            List.filterMap
                (\( lbl, v ) ->
                    case v of
                        Just x ->
                            if x < 0 then
                                Just (lbl ++ " can't be negative.")

                            else
                                Nothing

                        Nothing ->
                            Nothing
                )
                values

        rangeErrors =
            List.filterMap identity
                [ if inputs.price <= 0 then
                    Just "Home price must be greater than zero."

                  else
                    Nothing
                , if inputs.downA + inputs.downB >= inputs.price && inputs.price > 0 then
                    Just "Combined down payments must be less than the home price."

                  else
                    Nothing
                , if inputs.years < 1 || inputs.years > 50 then
                    Just "Loan term must be between 1 and 50 years."

                  else
                    Nothing
                , if sale.years < 1 || sale.years > inputs.years then
                    Just ("Years until sale must be between 1 and the loan term (" ++ String.fromInt inputs.years ++ ").")

                  else
                    Nothing
                ]
    in
    if not (List.isEmpty (unparseable ++ appreciationError)) then
        Err (unparseable ++ appreciationError)

    else if not (List.isEmpty (negatives ++ rangeErrors)) then
        Err (negatives ++ rangeErrors)

    else
        Ok { inputs = inputs, sale = sale }



-- FORMATTING


money : Float -> String
money x =
    let
        cents =
            round (abs x * 100)

        sign =
            if x < 0 && cents > 0 then
                "−"

            else
                ""
    in
    sign ++ "$" ++ withCommas (cents // 100) ++ "." ++ String.padLeft 2 '0' (String.fromInt (modBy 100 cents))


wholeMoney : Float -> String
wholeMoney x =
    let
        dollars =
            round (abs x)

        sign =
            if x < 0 && dollars > 0 then
                "−"

            else
                ""
    in
    sign ++ "$" ++ withCommas dollars


withCommas : Int -> String
withCommas n =
    if n < 1000 then
        String.fromInt n

    else
        withCommas (n // 1000) ++ "," ++ String.padLeft 3 '0' (String.fromInt (modBy 1000 n))


percent : Float -> String
percent x =
    String.replace "-" "−" (String.fromInt (round x)) ++ "%"


pluralMonths : Int -> String
pluralMonths n =
    if n == 1 then
        "1 month"

    else
        String.fromInt n ++ " months"


nameOf : Model -> Buyer -> String
nameOf model buyer =
    let
        ( raw, fallback ) =
            case buyer of
                A ->
                    ( model.nameA, "Co-buyer 1" )

                B ->
                    ( model.nameB, "Co-buyer 2" )
    in
    if String.isEmpty (String.trim raw) then
        fallback

    else
        String.trim raw


buyerKey : Buyer -> String
buyerKey buyer =
    case buyer of
        A ->
            "a"

        B ->
            "b"


downOf : Buyer -> Mortgage.Inputs -> Float
downOf buyer inputs =
    case buyer of
        A ->
            inputs.downA

        B ->
            inputs.downB


shareOf : Buyer -> Mortgage.Split -> Float
shareOf buyer s =
    case buyer of
        A ->
            s.a

        B ->
            s.b



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "page" ]
        [ header [ class "masthead" ]
            [ h1 [] [ text "Co-buyer mortgage calculator" ] ]
        , main_ [ class "layout" ]
            [ viewInputs model
            , viewResults model
            ]
        , footer [ class "notes" ]
            [ p []
                [ text "A co-buyer's contribution is their down payment plus the sum of their monthly payments over the full term. "
                , text "The home is owned 50/50. Whoever puts down more has covered part of the other's half of the down payment; that amount is treated as an internal loan, repaid through a fixed extra monthly payment on top of an even split of the bill, so the two payments always add up to the bill. "
                , text "On a sale, the equity (sale price minus selling costs and the mortgage payoff) is split evenly and whatever is left of the internal loan is settled from the borrower's half. "
                , text "The comparison is in nominal dollars: it ignores the time value of money, home appreciation, closing costs, maintenance, and selling before the loan ends. "
                , text "PMI applies when the loan is above 80% of the price and stops once the scheduled balance reaches 78%."
                ]
            ]
        ]


viewInputs : Model -> Html Msg
viewInputs model =
    section [ class "panel inputs" ]
        [ h2 [] [ text "Co-buyers" ]
        , div [ class "buyers" ]
            [ viewBuyerInputs model A DownA model.downA
            , viewBuyerInputs model B DownB model.downB
            ]
        , div [ class "grid single" ]
            [ numberField "Interest on the down payment difference" "" "% / yr" InternalRate model.internalRate ]
        , h2 [] [ text "Home & loan" ]
        , div [ class "grid" ]
            [ numberField "Home price" "$" "" Price model.price
            , numberField "Interest rate" "" "%" Rate model.rate
            , numberField "Loan term" "" "years" Years model.years
            , numberField "Property tax" "" "% / yr" TaxRate model.taxRate
            , numberField "Home insurance" "$" "/ yr" Insurance model.insurance
            , numberField "HOA dues" "$" "/ mo" Hoa model.hoa
            , numberField "PMI rate" "" "% / yr" PmiRate model.pmiRate
            ]
        , h2 [] [ text "Selling early" ]
        , div [ class "grid three" ]
            [ numberField "Sell after" "" "years" SellYears model.sellYears
            , numberField "Appreciation" "" "% / yr" Appreciation model.appreciation
            , numberField "Selling costs" "" "%" SellingCosts model.sellingCosts
            ]
        ]


viewBuyerInputs : Model -> Buyer -> Field -> String -> Html Msg
viewBuyerInputs model buyer downField downValue =
    div [ class ("buyer buyer-" ++ buyerKey buyer) ]
        [ label [ class "field" ]
            [ span [ class "field-label" ] [ text "Name" ]
            , div [ class "input-wrap" ]
                [ input
                    [ type_ "text"
                    , value
                        (case buyer of
                            A ->
                                model.nameA

                            B ->
                                model.nameB
                        )
                    , onInput (SetName buyer)
                    ]
                    []
                ]
            ]
        , numberField "Down payment" "$" "" downField downValue
        ]


numberField : String -> String -> String -> Field -> String -> Html Msg
numberField labelText prefix suffix field current =
    label [ class "field" ]
        [ span [ class "field-label" ] [ text labelText ]
        , div [ class "input-wrap" ]
            [ affix "prefix" prefix
            , input
                [ type_ "text"
                , attribute "inputmode" "decimal"
                , value current
                , onInput (SetField field)
                ]
                []
            , affix "suffix" suffix
            ]
        ]


affix : String -> String -> Html msg
affix kind s =
    if String.isEmpty s then
        text ""

    else
        span [ class kind ] [ text s ]


viewResults : Model -> Html Msg
viewResults model =
    case parse model of
        Err errors ->
            section [ class "panel results" ]
                [ h2 [] [ text "Check your inputs" ]
                , ul [ class "errors" ] (List.map (\e -> li [] [ text e ]) errors)
                ]

        Ok { inputs, sale } ->
            let
                summary =
                    Mortgage.summarize inputs
            in
            section [ class "results" ]
                [ viewSplit model inputs summary
                , viewSale model inputs (Mortgage.sell inputs summary sale)
                , viewBill inputs summary
                ]


viewSplit : Model -> Mortgage.Inputs -> Summary -> Html Msg
viewSplit model inputs summary =
    let
        offset =
            Mortgage.monthlyOffset inputs summary

        hasPmi =
            summary.pmiMonths > 0

        withPmi =
            Mortgage.split offset (summary.baseBill + summary.pmi)

        afterPmi =
            Mortgage.split offset summary.baseBill

        -- The bill is smallest after PMI ends, so that's where a payment
        -- would first go negative.
        anyNegative =
            afterPmi.a < 0 || afterPmi.b < 0

        lent =
            abs (Mortgage.internalLoan inputs)

        n =
            toFloat summary.months

        ( bigger, smaller ) =
            if Mortgage.internalLoan inputs >= 0 then
                ( A, B )

            else
                ( B, A )

        explanation =
            if abs offset < 0.005 then
                "The down payments are equal, so the bill is split evenly."

            else
                nameOf model bigger
                    ++ " covered "
                    ++ wholeMoney lent
                    ++ " of "
                    ++ nameOf model smaller
                    ++ "'s half of the down payment. "
                    ++ nameOf model smaller
                    ++ " repays it by paying "
                    ++ money (abs offset)
                    ++ " more each month than "
                    ++ nameOf model bigger
                    ++ " over "
                    ++ String.fromInt inputs.years
                    ++ " years"
                    ++ (if inputs.internalRate > 0 then
                            ", including "
                                ++ String.fromFloat inputs.internalRate
                                ++ "% interest ("
                                ++ wholeMoney (abs offset * n / 2 - lent)
                                ++ " over the full term)."

                        else
                            ". Each co-buyer contributes "
                                ++ wholeMoney (summary.totalCost / 2)
                                ++ " in total."
                       )

    in
    section [ class "panel hero" ]
        [ p [ class "eyebrow" ] [ text "Monthly split" ]
        , div [ class "split" ]
            [ splitTile model A hasPmi summary withPmi afterPmi
            , splitTile model B hasPmi summary withPmi afterPmi
            ]
        , p [ class "explain" ] [ text explanation ]
        , if anyNegative then
            p [ class "callout warn" ]
                [ strong [] [ text "The down payments are too uneven to balance with the bill alone. " ]
                , text
                    (nameOf model smaller
                        ++ " would need to cover the whole bill and also pay "
                        ++ nameOf model bigger
                        ++ " directly each month to make the contributions equal (shown as a negative payment above)."
                    )
                ]

          else
            text ""
        , viewContributions model inputs summary offset
        ]


viewSale : Model -> Mortgage.Inputs -> Mortgage.Sale -> Html msg
viewSale model inputs sale =
    let
        years =
            sale.months // 12

        ( lender, borrower ) =
            if sale.owedToA >= 0 then
                ( A, B )

            else
                ( B, A )

        settlement =
            if abs sale.owedToA < 0.5 then
                "There's no down payment difference left to settle, so the equity is split evenly."

            else
                nameOf model borrower
                    ++ " still owes "
                    ++ nameOf model lender
                    ++ " "
                    ++ wholeMoney (abs sale.owedToA)
                    ++ " of the down payment difference. That comes out of "
                    ++ nameOf model borrower
                    ++ "'s half of the equity and goes to "
                    ++ nameOf model lender
                    ++ "."

        row buyer =
            let
                ( paid, payout ) =
                    case buyer of
                        A ->
                            ( sale.paidA, sale.payoutA )

                        B ->
                            ( sale.paidB, sale.payoutB )
            in
            tr []
                [ td [] [ span [ class ("dot dot-" ++ buyerKey buyer) ] [], text (nameOf model buyer) ]
                , td [ class "num" ] [ text (wholeMoney paid) ]
                , td [ class "num strong" ] [ text (wholeMoney payout) ]
                , td [ class "num" ] [ text (wholeMoney (paid - payout)) ]
                ]

        shortfall =
            List.filterMap
                (\( buyer, payout ) ->
                    if payout < 0 then
                        Just (nameOf model buyer ++ " would need to bring " ++ wholeMoney -payout ++ " to closing. ")

                    else
                        Nothing
                )
                [ ( A, sale.payoutA ), ( B, sale.payoutB ) ]
    in
    section [ class "panel" ]
        [ h2 [] [ text ("If you sell after " ++ String.fromInt years ++ " years") ]
        , div [ class "facts flush" ]
            [ fact "Sale price" (wholeMoney sale.salePrice)
            , fact "Selling costs" (wholeMoney sale.sellingCosts)
            , fact "Mortgage payoff" (wholeMoney sale.mortgageBalance)
            , fact "Equity" (wholeMoney sale.equity)
            ]
        , p [ class "explain" ] [ text settlement ]
        , div [ class "split" ]
            [ payoutTile model A sale.payoutA
            , payoutTile model B sale.payoutB
            ]
        , if List.isEmpty shortfall then
            text ""

          else
            p [ class "callout warn" ] [ text (String.concat shortfall) ]
        , table [ class "contrib" ]
            [ thead []
                [ tr []
                    [ th [] [ text "" ]
                    , th [ class "num" ] [ text "Put in" ]
                    , th [ class "num" ] [ text "Gets back" ]
                    , th [ class "num" ] [ text "Net cost" ]
                    ]
                ]
            , tbody [] [ row A, row B ]
            ]
        , p [ class "tile-note table-note" ]
            [ text
                ("Put in is the down payment plus "
                    ++ String.fromInt years
                    ++ " years of monthly payments. "
                    ++ (if inputs.internalRate > 0 then
                            "Net cost differs by twice the interest paid on the down payment difference."

                        else
                            "Net cost is the same for both: the cost of owning the home together, shared equally."
                       )
                )
            ]
        ]


payoutTile : Model -> Buyer -> Float -> Html msg
payoutTile model buyer amount =
    div [ class ("tile tile-" ++ buyerKey buyer) ]
        [ span [ class "tile-name" ] [ text (nameOf model buyer ++ " receives") ]
        , span [ class "tile-value" ] [ text (wholeMoney amount) ]
        ]


splitTile : Model -> Buyer -> Bool -> Summary -> Mortgage.Split -> Mortgage.Split -> Html msg
splitTile model buyer hasPmi summary withPmi afterPmi =
    let
        payment =
            shareOf buyer afterPmi

        billShare =
            if summary.baseBill > 0 then
                payment / summary.baseBill * 100

            else
                0
    in
    div [ class ("tile tile-" ++ buyerKey buyer) ]
        [ span [ class "tile-name" ] [ text (nameOf model buyer) ]
        , span [ class "tile-value" ] [ text (money payment), span [ class "tile-unit" ] [ text " / mo" ] ]
        , span [ class "tile-note" ]
            [ text
                (if hasPmi then
                    percent billShare
                        ++ " of the bill after PMI ends · "
                        ++ money (shareOf buyer withPmi)
                        ++ " for the first "
                        ++ pluralMonths summary.pmiMonths

                 else
                    percent billShare ++ " of the monthly bill"
                )
            ]
        ]


viewContributions : Model -> Mortgage.Inputs -> Summary -> Float -> Html msg
viewContributions model inputs summary offset =
    let
        n =
            toFloat summary.months

        -- Each co-buyer's share of every monthly bill, summed over the term.
        monthlyTotal buyer =
            let
                sign =
                    case buyer of
                        A ->
                            -1

                        B ->
                            1
            in
            (summary.totalPaid + sign * offset * n) / 2

        row buyer =
            let
                down =
                    downOf buyer inputs

                paid =
                    monthlyTotal buyer
            in
            tr []
                [ td [] [ span [ class ("dot dot-" ++ buyerKey buyer) ] [], text (nameOf model buyer) ]
                , td [ class "num" ] [ text (wholeMoney down) ]
                , td [ class "num" ] [ text (wholeMoney paid) ]
                , td [ class "num strong" ] [ text (wholeMoney (down + paid)) ]
                ]

        bar buyer =
            let
                down =
                    max 0 (downOf buyer inputs)

                paid =
                    max 0 (monthlyTotal buyer)

                total =
                    down + paid

                pct x =
                    if total > 0 then
                        String.fromFloat (x / total * 100) ++ "%"

                    else
                        "0%"
            in
            div [ class "stack-row" ]
                [ span [ class "stack-name" ] [ text (nameOf model buyer) ]
                , div [ class "stack" ]
                    [ div [ class ("stack-seg down seg-" ++ buyerKey buyer), style "width" (pct down) ] []
                    , div [ class ("stack-seg paid seg-" ++ buyerKey buyer), style "width" (pct paid) ] []
                    ]
                ]
    in
    div [ class "contrib-wrap" ]
        [ table [ class "contrib" ]
            [ thead []
                [ tr []
                    [ th [] [ text "" ]
                    , th [ class "num" ] [ text "Down payment" ]
                    , th [ class "num" ] [ text "Monthly payments" ]
                    , th [ class "num" ] [ text "Total" ]
                    ]
                ]
            , tbody [] [ row A, row B ]
            ]
        , div [ class "stacks" ] [ bar A, bar B ]
        , div [ class "legend" ]
            [ span [] [ span [ class "swatch down" ] [], text "Down payment" ]
            , span [] [ span [ class "swatch paid" ] [], text "Monthly payments" ]
            ]
        ]


viewBill : Mortgage.Inputs -> Summary -> Html msg
viewBill inputs summary =
    let
        hasPmi =
            summary.pmiMonths > 0

        line labelText amount =
            tr [] [ td [] [ text labelText ], td [ class "num" ] [ text (money amount) ] ]

        totalLine labelText amount =
            tr [ class "total" ] [ td [] [ text labelText ], td [ class "num" ] [ text (money amount) ] ]
    in
    section [ class "panel" ]
        [ h2 [] [ text "Monthly mortgage bill" ]
        , table [ class "bill" ]
            [ tbody []
                (List.concat
                    [ [ line "Principal & interest" summary.principalAndInterest
                      , line "Property tax" summary.tax
                      , line "Home insurance" summary.insurance
                      ]
                    , if summary.hoa > 0 then
                        [ line "HOA dues" summary.hoa ]

                      else
                        []
                    , if hasPmi then
                        [ line ("PMI (first " ++ pluralMonths summary.pmiMonths ++ ")") summary.pmi
                        , totalLine "Total while PMI applies" (summary.baseBill + summary.pmi)
                        , totalLine "Total after PMI ends" summary.baseBill
                        ]

                      else
                        [ totalLine "Total" summary.baseBill ]
                    ]
                )
            ]
        , div [ class "facts" ]
            [ fact "Loan amount" (wholeMoney summary.loan)
            , fact "Down payment" (percent ((inputs.downA + inputs.downB) / inputs.price * 100))
            , fact "Total interest" (wholeMoney summary.totalInterest)
            , fact "Total cost of home" (wholeMoney summary.totalCost)
            ]
        ]


fact : String -> String -> Html msg
fact labelText v =
    div [ class "fact" ]
        [ span [ class "fact-label" ] [ text labelText ]
        , span [ class "fact-value" ] [ text v ]
        ]
