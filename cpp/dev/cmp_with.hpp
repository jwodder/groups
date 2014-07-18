/* Step 1: Give class `Foo` an `int cmp(const Foo&) const` method
 * Step 2: Make `Foo` subclass `cmp_with<Foo>`
 * Step 3: ???
 * Step 4: Reduced typing!
 */

template<class T>
struct cmp_with {
 bool operator==(const T& y) const {return x.cmp(y) == 0; }
 bool operator< (const T& y) const {return x.cmp(y) <  0; }
 bool operator> (const T& y) const {return x.cmp(y) >  0; }
 bool operator>=(const T& y) const {return x.cmp(y) >= 0; }
 bool operator<=(const T& y) const {return x.cmp(y) <= 0; }
 bool operator!=(const T& y) const {return x.cmp(y) != 0; }
};
