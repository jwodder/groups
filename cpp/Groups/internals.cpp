#include <cstdlib>
#include <ostream>
#include <string>
#include <vector>
#include "Groups/internals.hpp"
using namespace std;

namespace Groups {
 int gcd(int x, int y) {
  int a = abs(x), b = abs(y);
  if (a == 0 && b == 0) return 0;
  if (a == 0) return b;
  if (b == 0) return a;
  do {int r = a % b; a = b; b = r; } while (b != 0);
  return a;
 }

 int lcm(int x, int y) {return x == 0 || y == 0 ? 0 : abs(x*y)/gcd(x,y); }

 ostream& expgen(ostream& out, const string& gen, int n, const string& def) {
  if (n == 0) return out << def;
  else if (n == 1) return out << gen;
  else if (1 < n && n < 10) return out << gen << '^' << n;
  else return out << gen << "^{" << n << '}';
 }

 int factorial(int x) {
  int fac = 1;
  for (int i=2; i<=x; i++) fac *= i;
  return fac;
 }

 vector<int> vecN(int n) {
  if (n < 1) return vector<int>(0);
  vector<int> elems(n);
  int i=0;
  for (int& x: elems) {
   x = i++;
  }
  return elems;
 }

 int modulo(int a, int n) {
  // Because C++ doesn't handle taking the remainder of a negative number
  // correctly
  if (n == 0) return a;
  n = abs(n);
  int x = a % n;
  return x < 0 ? x+n : x;
 }
}
