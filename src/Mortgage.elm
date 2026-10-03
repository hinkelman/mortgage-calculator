module Mortgage exposing
    ( Inputs
    , Sale
    , SaleInputs
    , Split
    , Summary
    , internalLoan
    , monthlyOffset
    , sell
    , split
    , summarize
    )

{-| Pure mortgage math, kept separate from the UI.

The co-buyers own the home 50/50. Whoever puts down more has covered part of
the other's half of the down payment; that amount is treated as an internal
loan, repaid through a larger share of the monthly bill. "Contribution" means
a co-buyer's down payment plus every monthly payment they make.

-}


type alias Inputs =
    { price : Float
    , downA : Float
    , downB : Float
    , annualRate : Float -- percent, e.g. 6.5
    , years : Int
    , taxRate : Float -- percent of price per year
    , insurance : Float -- dollars per year
    , hoa : Float -- dollars per month
    , pmiRate : Float -- percent of original loan per year
    , internalRate : Float -- percent per year on the internal loan
    }


type alias Summary =
    { loan : Float
    , months : Int
    , principalAndInterest : Float
    , tax : Float -- monthly
    , insurance : Float -- monthly
    , hoa : Float -- monthly
    , pmi : Float -- monthly, while it applies
    , pmiMonths : Int
    , baseBill : Float -- monthly bill without PMI
    , totalInterest : Float
    , totalPaid : Float -- sum of all monthly bills over the term
    , totalCost : Float -- down payments + totalPaid
    }


summarize : Inputs -> Summary
summarize i =
    let
        loan =
            i.price - i.downA - i.downB

        n =
            i.years * 12

        r =
            i.annualRate / 1200

        pi =
            payment loan r n

        tax =
            i.price * i.taxRate / 1200

        ins =
            i.insurance / 12

        pmi =
            loan * i.pmiRate / 1200

        -- PMI is required when the loan exceeds 80% of the price and is
        -- dropped automatically once the scheduled balance reaches 78%.
        pmiMonths =
            if loan > 0.8 * i.price && pmi > 0 then
                countMonthsAbove (0.78 * i.price) pi r n loan 0

            else
                0

        baseBill =
            pi + tax + ins + i.hoa

        totalPaid =
            toFloat n * baseBill + toFloat pmiMonths * pmi
    in
    { loan = loan
    , months = n
    , principalAndInterest = pi
    , tax = tax
    , insurance = ins
    , hoa = i.hoa
    , pmi = pmi
    , pmiMonths = pmiMonths
    , baseBill = baseBill
    , totalInterest = pi * toFloat n - loan
    , totalPaid = totalPaid
    , totalCost = i.downA + i.downB + totalPaid
    }


{-| Level monthly payment that amortizes `principal` over `n` months.
-}
payment : Float -> Float -> Int -> Float
payment principal r n =
    if n <= 0 then
        0

    else if r == 0 then
        principal / toFloat n

    else
        principal * r / (1 - (1 + r) ^ toFloat -n)


{-| Balance remaining after `k` level payments.
-}
balanceAfter : Float -> Float -> Float -> Int -> Float
balanceAfter principal r pmt k =
    if r == 0 then
        principal - pmt * toFloat k

    else
        principal * (1 + r) ^ toFloat k - pmt * ((1 + r) ^ toFloat k - 1) / r


countMonthsAbove : Float -> Float -> Float -> Int -> Float -> Int -> Int
countMonthsAbove threshold pmt r monthsLeft balance count =
    if monthsLeft <= 0 || balance <= threshold then
        count

    else
        countMonthsAbove threshold
            pmt
            r
            (monthsLeft - 1)
            (balance * (1 + r) - pmt)
            (count + 1)



-- INTERNAL LOAN


{-| What A has covered of B's half of the down payment. Negative means B
covered part of A's half.
-}
internalLoan : Inputs -> Float
internalLoan i =
    (i.downA - i.downB) / 2


{-| Monthly repayment of the internal loan, signed like `internalLoan`.
-}
monthlyTransfer : Inputs -> Summary -> Float
monthlyTransfer i s =
    signed (internalLoan i) (payment (abs (internalLoan i)) (i.internalRate / 1200) s.months)


signed : Float -> Float -> Float
signed like x =
    if like < 0 then
        -x

    else
        x


{-| How much more B pays each month than A. B pays half the bill plus the
transfer, A pays half the bill minus it, so the gap is twice the transfer.
-}
monthlyOffset : Inputs -> Summary -> Float
monthlyOffset i s =
    2 * monthlyTransfer i s


type alias Split =
    { a : Float
    , b : Float
    }


{-| Divide one month's bill. The two shares always sum to the bill.
-}
split : Float -> Float -> Split
split offset bill =
    { a = (bill - offset) / 2
    , b = (bill + offset) / 2
    }



-- SELLING EARLY


type alias SaleInputs =
    { years : Int
    , appreciation : Float -- percent per year
    , sellingCosts : Float -- percent of sale price
    }


type alias Sale =
    { months : Int
    , salePrice : Float
    , sellingCosts : Float
    , mortgageBalance : Float
    , equity : Float
    , owedToA : Float -- internal loan still outstanding; negative means owed to B
    , paidA : Float -- down payment + monthly payments through the sale
    , paidB : Float
    , payoutA : Float
    , payoutB : Float
    }


{-| Sell after `years`: split the equity 50/50, then settle what's left of
the internal loan out of the shares.
-}
sell : Inputs -> Summary -> SaleInputs -> Sale
sell i s sale =
    let
        k =
            min s.months (sale.years * 12)

        salePrice =
            i.price * (1 + sale.appreciation / 100) ^ toFloat sale.years

        costs =
            salePrice * sale.sellingCosts / 100

        mortgageBalance =
            max 0 (balanceAfter s.loan (i.annualRate / 1200) s.principalAndInterest k)

        equity =
            salePrice - costs - mortgageBalance

        lent =
            internalLoan i

        transfer =
            monthlyTransfer i s

        owed =
            signed lent
                (max 0 (balanceAfter (abs lent) (i.internalRate / 1200) (abs transfer) k))

        billsPaid =
            toFloat k * s.baseBill + toFloat (min k s.pmiMonths) * s.pmi
    in
    { months = k
    , salePrice = salePrice
    , sellingCosts = costs
    , mortgageBalance = mortgageBalance
    , equity = equity
    , owedToA = owed
    , paidA = i.downA + billsPaid / 2 - toFloat k * transfer
    , paidB = i.downB + billsPaid / 2 + toFloat k * transfer
    , payoutA = equity / 2 + owed
    , payoutB = equity / 2 - owed
    }
