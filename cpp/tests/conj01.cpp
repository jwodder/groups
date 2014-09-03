#include <iostream>
#include <iomanip>  /* setw */
#include <ios>  /* left */
#include "Groups/Families/Dicyclic.hpp"
#include "Groups/Group.hpp"
using namespace std;
using namespace Groups;

int main() {
 Group g(Quaternion());
 vector<Element> elems = g.elements();
 vector<Element>::const_iterator xiter, yiter;
 size_t maxLen = 0;
 for (xiter = elems.begin(); xiter != elems.end(); xiter++) {
  if (string(*xiter).size() > maxLen) maxLen = string(*xiter).size();
 }
 cout << left;
 cout << string(maxLen, ' ');
 for (yiter = elems.begin(); yiter != elems.end(); yiter++) {
  cout << '|' << setw(maxLen) << *yiter;
 }
 cout << endl;
 string tmp = string(maxLen, '-');
 cout << tmp;
 tmp = "|" + tmp;
 for (int i=0; i<g.order(); i++) cout << tmp;
 cout << endl;
 for (xiter = elems.begin(); xiter != elems.end(); xiter++) {
  cout << setw(maxLen) << *xiter;
  for (yiter = elems.begin(); yiter != elems.end(); yiter++) {
   cout << '|' << setw(maxLen) << g.conjugate(*xiter, *yiter);
  }
  cout << endl;
 }
 return 0;
}
