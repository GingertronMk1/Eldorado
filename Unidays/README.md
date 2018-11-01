# Unidays Coding Challenge

This is a C++ program designed to meet the requirements for the UNiDAYS tech placements challenge.

I went for an object-oriented approach only for the challenge part itself; given that the pricing rules detailed the values of the basket items I elected to keep those items as just characters (this does mean that only single-character item names are permitted).

For the pricing rules, and their being passed into the `UnidaysDiscountChallenge` class, I went for some simple string matching for the three kinds of offers in the challenge: None, X for £Y, and X for the price of Y. These rules are passed in via a vector of strings, which are of the form `<Item Kind>:£<Price for 1>;<Offer>`. Hence the offers described in the challenge are formatted:

    "A:£8;No offer"
    "B:£12;2 for £20"
    "C:£4;3 for £10"
    "D:£7;2 for the price of 1"
    "E:£5;3 for the price of 2"

To run: download the C++ file, modify the price rules as you see fit, and compile with your favourite C++ compiler (I used G++ on a Mac).

If an item in the basket is not in the pricing rules, it will throw an error about it, and not return a value for the basket.
