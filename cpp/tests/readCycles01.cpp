#include <iostream>
#include <stdexcept>
#include <string>
#include "Permutation.hpp"
using namespace std;

string goodStrings[] = {"  1 ", "  1", "1", "( 1 2)", "(2)", "(1 2)  ( 3 5)",
			"(1 3 4)(2 7)", "(1 3 4)(5)(2 7)"};

string badStrings[] = {"  1 1", "(1 -5)", "(1 2) 3", "(2 -3 5)", "(1 2 1)",
		       "6", "(1 (3 ) 4)", "(5 7) () (8 9)", "(1 2", "(1 2 "};

int main(void) {
 for (size_t i=0; i < sizeof(goodStrings)/sizeof(goodStrings[0]); i++) {
  Permutation p = Permutation::parse(goodStrings[i]);
  cout << p << endl;
 }
 for (size_t i=0; i < sizeof(badStrings)/sizeof(badStrings[0]); i++) {
  try {
   Permutation p = Permutation::parse(badStrings[i]);
   cout << "Unexpected success: " << string(p) << endl;
  } catch (invalid_argument& e) {
   cout << "Received expected invalid_argument: " << e.what() << endl;
  }
 }
 return 0;
}
