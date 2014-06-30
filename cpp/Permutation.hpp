#ifndef PERMUTATION_H
#define PERMUTATION_H

#include <map>
#include <ostream>
#include <stdexcept>  /* invalid_argument */
#include <string>
#include <vector>

#if defined(__cplusplus) && __cplusplus >= 201103L
#include <iterator>  /* begin, end */
#endif

/**
  * A Permutation object represents a permutation of finitely many positive
  * integers, i.e., a bijective function from some integer range `[1,n]` to
  * itself.
  **/

class Permutation {
public:

 /**
   * The default constructor evaluates to the identity permutation.
   **/
 Permutation();

 /**
   * Returns the largest integer that the Permutation permutes (does not map to
   * itself), or 0 if there is no such integer (i.e., if the Permutation is the
   * identity).
   *
   * @return  the degree of the Permutation
   **/
 int  degree() const;

 /**
   * Returns the order of the Permutation, i.e., the smallest positive integer
   * `n` such that multiplying `n` copies of the Permutation together produces
   * the identity
   *
   * @return  the order of the Permutation
   **/
 int  order()  const;

 /**
   * Tests whether the Permutation is even, i.e., can be expressed as the
   * product of an even number of transpositions
   *
   * @return  `true` if the Permutation is even, `false` otherwise
   **/
 bool isEven() const;

 /**
   * Tests whether the Permutation is odd, i.e., not even.  This function
   * returns the negation of isEven().
   *
   * @return  `true` if the Permutation is odd, `false` otherwise
   **/
 bool isOdd()  const;

 /**
   * Returns the sign (a.k.a. signature) of the Permutation, i.e., 1 if it is
   * even, -1 if it is odd
   *
   * @return  the sign of the Permutation
   **/
 int  sign()   const;

 /**
   * Encodes the Permutation as an nonnegative integer using a modified form of
   * Lehmer codes.  This encoding establishes a bijection between Permutation
   * values and nonnegative integers, with fromLehmer() converting values in
   * the opposite direction.
   *
   * Note that, on systems where int is 32 bits long, only Permutations with
   * degree() less than 13 can reliably have their modified Lehmer codes
   * calculated.  On 64-bit systems, this limit is raised to degrees less than
   * 21.
   *
   * @return  the modified Lehmer code for the Permutation
   **/
 int  lehmer() const;

 /**
   * Returns the inverse of the Permutation
   *
   * @return  the inverse of the Permutation
   **/
 Permutation inverse() const;

 /**
   * Decomposes the Permutation into a product of disjoint cycles.  toCycles()
   * returns a vector of cycles in which each cycle is a vector<int>.  Each
   * cycle `c` is a sub-permutation that maps `c[0]` to `c[1]`, `c[1]` to
   * `c[2]`, etc., finally mapping `c[c.size()-1]` back around to `c[0]`.  The
   * Permutation is then the product of these cycles.
   *
   * Each cycle is at least 2 elements in size and places its smallest element
   * at index 0.  Cycles are ordered by their index 0's in increasing order.
   * No two cycles share an element.
   *
   * When the Permutation is the identity, toCycles() returns an empty vector<
   * vector<int> >.
   *
   * @return  the cycle decomposition of the Permutation
   **/
 std::vector< std::vector<int> > toCycles() const;

 /**
   * Tests whether the Permutation and `other` are disjoint, i.e., whether they
   * do not permute any of the same integers
   *
   * @param other  a Permutation to compare against
   *
   * @return  `true` if the Permutation and `other` are disjoint, `false`
   *          otherwise
   **/
 bool disjoint(const Permutation&) const;

 /**
   * Returns a vector<int> of the results of applying the Permutation to the
   * integers 1 through degree().  If `v = p.toImage()`, then `v[0] == p(1)`,
   * `v[1] == p(2)`, etc.
   *
   * When the Permutation is the identity, toImage() returns an empty
   * vector<int>.
   *
   * @return  the image of 1 through degree() under the Permutation
   **/
 std::vector<int> toImage() const;

 /**
   * Applies the Permutation to `x`, returning the value that the Permutation
   * maps `x` to.  Values less than 1 are returned unchanged.
   *
   * @param x  an integer
   *
   * @return  the image of `x` under the Permutation
   **/
 int operator()(int) const;

 /**
   * Multiplication/composition of Permutations.  `p * q` returns a Permutation
   * `r` such that `p(q(x)) == r(x)` for all integers `x`.
   *
   * @param other  a Permutation to multiply by
   *
   * @return  the product of the Permutation and `other`
   **/
 Permutation operator*(const Permutation&) const;

 /**
   * In-place multiplication
   *
   * @param other  a Permutation to multiply by
   *
   * @return  a reference to the Permutation
   **/
 Permutation& operator*=(const Permutation&);

 /**
   * Sets the Permutation to the value with the next highest modified Lehmer
   * code; i.e., if `x = p.lehmer()`, then after performing `++p`, `p.lehmer()`
   * will equal `x+1`.  This new value is also the least Permutation greater
   * than the old value.
   *
   * The modified Lehmer code is not actually computed, and so this will
   * continue to work when degree() becomes arbitrarily large.
   *
   * @return  a reference to the Permutation
   **/
 Permutation& operator++();

 /**
   * Postfix increment.  `p++` performs `++p` and returns the value `p` had
   * before incrementing.
   *
   * @return  the old value of the Permutation
   **/
 Permutation  operator++(int);

 /**
   * Sets the Permutation to the value with the next lowest modified Lehmer
   * code; i.e., if `x = p.lehmer()`, then after performing `--p`, `p.lehmer()`
   * will equal `x-1`.  This new value is also the greatest Permutation less
   * than the old value.
   *
   * The modified Lehmer code is not actually computed, and so this will
   * continue to work when degree() becomes arbitrarily large.
   *
   * @return  a reference to the Permutation
   *
   * @exception logic_error  thrown if operator--() is called when lehmer() is
   *                         0, i.e., when the Permutation is the identity
   **/
 Permutation& operator--();

 /**
   * Postfix decrement.  `p--` performs `--p` and returns the value `p` had
   * before decrementing.
   *
   * @return  the old value of the Permutation
   **/
 Permutation  operator--(int);

 /**
   * Converts a Permutation to cycle notation.  The Permutation is decomposed
   * into cycles with toCycles(), each cycle is written as a parenthesized
   * space-separated sequence of integers, and the cycles are concatenated.
   *
   * When called on the identity, operator string() returns `"1"`.
   *
   * @return  the representation of the Permutation in cycle notation
   **/
 operator std::string() const;

 /**
   * Tests whether the Permutation is not the identity
   *
   * @return  `true` if Permutation does not equal identity(), `false` otherwise
   **/
 operator bool() const;

 /**
   * Returns the identity Permutation.  This is a more instantly-grokkable way
   * of writing Permutation().
   *
   * @returns  the identity Permutation
   **/
 static Permutation identity();

 /**
   * Returns the Permutation corresponding to the given modified Lehmer code.
   * This is the inverse of lehmer().
   *
   * @param x  a nonnegative integer
   *
   * @return  the Permutation with modified Lehmer code `x`
   *
   * @exception invalid_argument  thrown if `x` is less than 0
   **/
 static Permutation fromLehmer(int);

 /**
   * Returns the transposition of `x` and `y`, i.e., the Permutation that maps
   * `x` to `y`, maps `y` to `x`, and leaves all other values unchanged.  When
   * `x` equals `y`, transposition() returns identity().
   *
   * @param x  a positive integer
   * @param y  a positive integer
   *
   * @return  the transposition of `x` and `y`
   *
   * @exception invalid_argument  thrown when `x` or `y` is less than 1
   **/
 static Permutation transposition(int, int);

 /**
   * Returns the Permutation with degree() `n` that has the smallest modified
   * Lehmer code of all Permutations with degree() `n`.  If `n` is 0 or 1 (or
   * anything less than 0), this is identity().  For higher degrees, this is
   * `transposition(n, n-1)`.
   *
   * @param n  the degree of the Permutation to return
   *
   * @return  the first Permutation of degree `n` in modified Lehmer code order
   **/
 static Permutation firstOfDegree(int);

 /**
   * Constructs a Permutation from a vector<int> that specifies the results of
   * applying the target Permutation to the integers 1 through some `n`.  If `p
   * = Permutation::fromImage(v)`, then `p(1) == v[0]`, `p(2) == v[1]`, etc.
   *
   * When `v` is empty, fromImage() returns identity().
   *
   * @param v  the image of the Permutation to construct
   *
   * @return  the Permutation with image `v`
   *
   * @exception invalid_argument  thrown if `v` contains a value less than 1,
   *                              contains the same value more than once, or
   *                              does not contain every integer value from 1
   *                              through `v.size()`
   **/
 static Permutation fromImage(const std::vector<int>&);

 /**
   * Returns a vector<Permutation> of all Permutations in the symmetric group
   * of degree `n`, i.e., all Permutations with degree() less than or equal to
   * `n`, i.e., all Permutations from identity() up to but not including
   * `firstOfDegree(n+1)`.  The Permutations are ordered by their modified
   * Lehmer codes in ascending order.
   *
   * Note that the size of the returned vector is `n!`, which can be very large
   * even for small values of `n`.
   *
   * @param n  a nonnegative integer
   *
   * @return  a vector of all Permutations with degree() `n` or less
   *
   * @exception invalid_argument  thrown if `n` is less than 0
   **/
 static std::vector<Permutation> s_n(int);

 /**
   * Parses a Permutation value written in cycle notation, possibly with added
   * whitespace.  This is the inverse of operator string().
   *
   * @param s  a string representing a Permutation in cycle notation
   *
   * @return  the Permutation represented by `s`
   *
   * @exception invalid_argument  thrown if `s` is not a valid representation
   *                              of a Permutation
   **/
 static Permutation parse(const std::string&);

 /**
   * Returns 1 if the Permutation is greater than `other`, -1 if it is less
   * than `other`, and 0 if they are equal.
   *
   * The ordering used by both cmp() and the comparison operators is such that
   * `p < q` if & only if `p.lehmer() < q.lehmer()`.  Note that none of these
   * methods actually compute any modified Lehmer codes, and so they will
   * continue to work on Permutations of arbitarily large degree().
   *
   * @param other  a Permutation to compare against
   *
   * @return  1, 0, or -1
   **/
 int cmp(const Permutation&) const;

 bool operator==(const Permutation& y) const {return cmp(y) == 0; }
 bool operator<(const Permutation& y)  const {return cmp(y) <  0; }
 bool operator>(const Permutation& y)  const {return cmp(y) >  0; }
 bool operator>=(const Permutation& y) const {return cmp(y) >= 0; }
 bool operator<=(const Permutation& y) const {return cmp(y) <= 0; }
 bool operator!=(const Permutation& y) const {return cmp(y) != 0; }

 /**
   * Equivalent to `fromImage(vector<int>(first, last))`
   *
   * @tparam Iter  an input iterator over int values
   * @param first  iterator to the beginning of an integer sequence
   * @param last   iterator to the end of an integer sequence
   *
   * @return  the Permutation with the given sequence as its image
   *
   * @exception invalid_argument  See fromImage(const vector<int>&)
   **/
 template<class Iter>
 static Permutation fromImage(Iter first, Iter last) {
  return fromImage(std::vector<int>(first, last));
 }

 /**
   * Constructs the Permutation represented by the cycle of ints given by the
   * contents of the range `[first, last)`.  If `p =
   * Permutation::fromCycle(first, last)`, then `p(*first) == *(first+1)`,
   * p(*(first+1)) == *(first+2)`, etc., and `p(*(last-1)) == *first`.  `p`
   * returns all other values unchanged.
   *
   * When `first == last`, identity() is returned.
   *
   * @tparam Iter  an input iterator over int values
   * @param first  iterator to the beginning of an integer sequence
   * @param last   iterator to the end of an integer sequence
   *
   * @return  the Permutation represented by the given cycle
   *
   * @exception invalid_argument  thrown if `[first, last)` contains a value
   *                              less than 1 or contains the same value more
   *                              than once
   **/
 template<class Iter>
 static Permutation fromCycle(Iter first, Iter last) {
  if (first == last) return Permutation();
  std::map<int,int> mapping;
  int start = *first, maxVal = 0;
  while (first != last) {
   int v = *first;
   if (v < 1) throw std::invalid_argument("Permutation::fromCycle: values must be positive");
   if (mapping.find(v) != mapping.end()) throw std::invalid_argument("Permutation::fromCycle: value appears more than once in cycle");
   first++;
   mapping[v] = first == last ? start : *first;
   if (v > maxVal) maxVal = v;
  }
  if (mapping.size() < 2) return Permutation();
  std::vector<int> vmap(maxVal);
  for (int i=0; i<maxVal; i++) {
   int j = mapping[i+1];
   vmap[i] = j==0 ? i+1 : j;
  }
  return Permutation(vmap, mapping.size() % 2, mapping.size());
 }

 /**
   * Constructs the Permutation represented by a product of cycles.
   * Dereferencing each iterator in the range `[first, last)` must produce an
   * object with `begin()` and `end()` methods (or, when `__cplusplus` is at
   * least `201103L`, that can be passed to std::begin and std::end) that
   * return iterators that can be passed to fromCycle().  fromCycles() then
   * multiplies the return values of each fromCycle() call together and returns
   * the result.
   *
   * When `first == last`, identity() is returned.
   *
   * @tparam Iter  an input iterator over values with `begin` and `end` methods
   * @param first  iterator to the beginning of a sequence of cycles
   * @param last   iterator to the end of a sequence of cycles
   *
   * @return  the Permutation represented by the product of the cycles
   *
   * @exception invalid_argument  may be thrown by calls to fromCycle()
   **/
 template<class Iter>
 static Permutation fromCycles(Iter first, Iter last) {
  Permutation p;
  while (first != last) {
#if defined(__cplusplus) && __cplusplus >= 201103L
   p *= fromCycle(std::begin(*first), std::end(*first));
#else
   p *= fromCycle(first->begin(), first->end());
#endif
   first++;
  }
  return p;
 }

private:
 Permutation(const std::vector<int>&, int=-1, int=-1, int=-1);
 std::vector<int> pmap;
 mutable int _even, _order, _lehmer;
};

/**
  * Equivalent to `o << string(p)`
  *
  * @param o  an output stream
  * @param p  a Permutation
  *
  * @return  `o`
  **/
inline std::ostream& operator<<(std::ostream& o, const Permutation& p) {
 return o << std::string(p);
}

#endif
