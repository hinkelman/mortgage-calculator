# Co-buyer Mortgage Calculator

An Elm 0.19.2 app for two people buying a home together, where one may bring a
bigger down payment and the other may have more monthly cash flow. Enter each
co-buyer's down payment along with the home price and loan terms, and it splits
the monthly mortgage bill so both have contributed the same total (down
payment plus all monthly payments) by the end of the loan.

**Try it:** https://hinkelman.github.io/mortgage-calculator/

## Build and run

```bash
elm make src/Main.elm --optimize --output=main.js
```

Then open `index.html` in a browser.

## How it works

The home is owned 50/50. With down payments `dA` and `dB`, the co-buyer who
put down more has covered `L = (dA − dB) / 2` of the other's half. That is
treated as an internal loan, repaid over the `n`-month term at an optional
interest rate `i`:

    t = L·i / (1 − (1 + i)^−n)      (t = L / n when i = 0)

Each month's bill `M` is split evenly, with the borrower also paying `t`:

    pA = M/2 − t,   pB = M/2 + t

The two payments always add up to the bill, even when it changes as PMI drops
off. At 0% interest, both co-buyers have contributed the same total
(down payment + all monthly payments) by the end of the term. The bill includes
principal, interest, property tax, insurance, HOA, and PMI until the balance
reaches 78% of the price.

### Selling early

If the home is sold after `k` months:

    equity  = sale price − selling costs − mortgage balance
    payoutA = equity/2 + remaining internal loan
    payoutB = equity/2 − remaining internal loan

At 0% interest, each co-buyer's net cost (what they put in minus what they get
back) comes out the same.

- `src/Mortgage.elm` – the math
- `src/Main.elm` – the UI
