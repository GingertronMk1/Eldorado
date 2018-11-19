#ifndef Unidays_H
#define Unidays_H

#include <iostream>
#include <cstdio>
#include <vector>

using namespace std;

struct totalPrice {
  float total;
  float deliveryCharge;
};

class UnidaysDiscountChallenge {
  private:
    // Each item in the basket will be represented by a `char` denoting what kind of item it is
    string basket;
    // A list of strings that determine the pricing rules
    vector<string> priceRules;
    // A function that takes into account the price rules given to it, and applies it to a modified list of basket items
    float priceRulesF(char kind, int amount);
  public:
    // New constructor to provide the pricing rules on objet creation
    UnidaysDiscountChallenge(vector<string> pRules) : priceRules(pRules) {};
    // Adding a new item to the basket
    void addToBasket(char bi);
    // We can use an overload to add multiple items to the basket if necessary
    void addToBasket(string kinds);
    // Calculating the total price and returning it, via printing it broken down into items + delivery
    totalPrice calculateTotalPrice();
    // Printing the basket, somewhat prettily
    void printBasket();
    // Emptying the basket (uncommon in online shopping annoyingly)
    void clearBasket();
};

#endif
