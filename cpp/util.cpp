/* Internal utility functions */

#include <cstdlib>
#include <string>
#include <ostream>
using namespace std;

namespace Groups {
 int gcd(int x, int y) {
  int a = abs(x), b = abs(y);
  do {int r = a % b; a = b; b = r; } while (b != 0);
  return a;
 }

 int lcm(int x, int y) {return x == 0 || y == 0 ? 0 : abs(x*y)/gcd(x, y); }

 ostream& expgen(ostream& out, const string& gen, int n, const string& def) {
  if (n == 0) return out << def;
  else if (n == 1) return out << gen;
  else return out << gen << "^{" << n << '}';
 }
}
