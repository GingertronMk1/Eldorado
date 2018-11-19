#include <iostream>
#include <cstdio>
#include <vector>
#include <string>
#include <fstream>
#include "Unidays.hpp"

using namespace std;

/*  Price rules are defined by a string for each item class
 *  There are 3 types of offers as defined by the challenge:
 *  - No offer
 *  - X for Y (the pound sign is omitted here)
 *  - X for the price of Y
 *  Price rules are therefore given as a vector of strings
 */

vector<tuple<char, int>> countUniqueChars(string s) {
  vector<tuple<char, int>> u;                                       // Empty vector in which we'll store the new tuples
  for (char k : s) {                                                // For each item in the basket
    u.push_back(tuple<char, int>(k, count(s.begin(), s.end(), k))); // Create a tuple containing that item and the number of them therein
  }
  sort(u.begin(), u.end());                                         // Sort that list of tuples
  u.erase(unique(u.begin(), u.end()), u.end());                     // Pare it down to just the unique values
  return u;                                                         // Return that
}


void UnidaysDiscountChallenge::printBasket() {
  for (char b : basket) {
    cout << b << " ";      // Printing each item on its own line
  }
  cout << endl;
};

void UnidaysDiscountChallenge::addToBasket(char bi) {
  basket.push_back(bi);     // Adding one character to the back of the basket
}

void UnidaysDiscountChallenge::addToBasket(string kinds) {
  basket.append(kinds);     // Appending an entire string to the basket
}

void UnidaysDiscountChallenge::clearBasket() {
  basket.clear();           // Clearing the basket
}

totalPrice UnidaysDiscountChallenge::calculateTotalPrice() {
  vector<tuple<char, int>> newBasket = countUniqueChars(basket);  // Applying countUniqueChars to condense the basket
  float currValue = 0;        // At the beginning the basket has no value
  float delivery = 0;         // And the delivery of an empty basket is 0
  for (tuple<char, int> t : newBasket) {    // For each value in the condensed basket
    char kind = get<0>(t);                  // Take the item type
    char amount = get<1>(t);                // And the number of them
    try {
    currValue += priceRulesF(kind, amount); // And let the pricing rules figure out what to do with them
    } catch (const string msg) {
      throw msg;
      return {1000, 1000};
    }
  }
  (0 < currValue && currValue < 50) ? delivery = 7 : delivery = 0;  // If 0 <= basket value <= 50, bump the delivery to £7, otherwise don't
  totalPrice t = {currValue, delivery};
  return t;
}

float UnidaysDiscountChallenge::priceRulesF(char kind, int amount) {
  float v = 0;                                                  // This will be the amount for these items
  for (string r : priceRules) {                                 // For each rule:
    if (kind == r.front()) {                                    // If it's the rule relevant to the item we're looking at:
      string thisRule = r.erase(0,4);                           // Drop the "<kind>:£" bit
      size_t sep = thisRule.find(';');                          // Find the ';' character that separates the single value from the offer
      string offer = thisRule.substr(sep+1,thisRule.length());  // Locate the offer's string
      float singlePrice = atof(thisRule.substr(0,sep).c_str()); // Calculate the value of a single item
      size_t firstSpace = offer.find(' ');                      // Find the first space in the offer
      string x_s, y_s;  // Now some empty var declarations
      float x_f, y_f;   // These will be filled differently
      int x_i, y_i;     // Depending on the type of offer
      if(offer.compare("No offer") == 0) {  // If there's no special offer
        v += singlePrice*amount;            // Return the price for one multiplied by the number of them
        return v;
      } else if(offer.find(" for the price of ") != string::npos) { // If the string " for the price of " exists within the offer
        x_s = offer.substr(0,firstSpace);                           // The string denoting X
        y_s = offer.substr(firstSpace+18,offer.length()-1);         // The string denoting Y
        x_f = atof(x_s.c_str());    // The float value of X
        y_f = atof(y_s.c_str());    // The float value of Y
        x_i = (int)x_f;   // The int value of X
        y_i = (int)y_f;   // The int value of Y
        if(amount%x_i == 0) { // If there are exactly n*X items in the basket
                v += (singlePrice*amount*y_f)/x_f;  // It's Y/X times the amount times the price for one
              } else {    // Otherwise
                v += singlePrice*(amount%x_i);      // Take the top off
                v += (singlePrice*(amount - amount%x_i)*y_f)/x_f; // And for the rest do the bit from before
              };
        return v;
      } else if (offer.find(" for ") != string::npos) {     // X for £Y
        x_s = offer.substr(0, firstSpace);                  // See before for these
        y_s = offer.substr(firstSpace+7, offer.length()-1);
        x_f = atof(x_s.c_str());
        y_f = atof(y_s.c_str());
        x_i = (int)x_f;
        y_i = (int)y_f;
        v += y_f*(amount/x_i);
        v += singlePrice*(amount%x_i);
        return v;
      }
    }
  }
  string errorMsg = "Item ";
  errorMsg.push_back(kind);
  errorMsg += " not in pricing rules.";
  throw errorMsg;
}

int main () {
  vector<string> prs;
  ifstream rulesFile("rules.txt");
  string line;
  if(rulesFile.is_open()) {
    while(getline(rulesFile, line)) {
      cout << line << endl;
      prs.push_back(line);
    }
    rulesFile.close();
  } else {
    cout << "Error, could not open file" << endl;
  }
  UnidaysDiscountChallenge udc(prs);
  totalPrice t;
  vector<string> testing {"",
                          "A",
                          "B",
                          "C",
                          "D",
                          "E",
                          "BB",
                          "BBB",
                          "BBBB",
                          "CCC",
                          "CCCC",
                          "DD",
                          "EBDFD",
                          "DDD",
                          "EE",
                          "EEE",
                          "EEEE",
                          "DDDDDDDDDDDDDD",
                          "BBBBCCC",
                          "ABBCCCDDEE",
                          "EDCBAEDCBC"
  };
  for (string s : testing) {
    udc.addToBasket(s);
    try {
    t = udc.calculateTotalPrice();
    udc.printBasket();
    cout << t.total << "\t" << t.deliveryCharge << endl;
    } catch (const string msg) {
      cout << msg << endl;
    }
    udc.clearBasket();
    cout << endl;
  }
  return 0;
}
