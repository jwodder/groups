/* These interfere with normal comparison operators for some reason, preventing
 * uses of classes' pre-existing operators from compiling. */

 template<class T>
 bool operator==(const T& x, const T& y) {return x.cmp(y) == 0; }

 template<class T>
 bool operator< (const T& x, const T& y) {return x.cmp(y) < 0; }

 template<class T>
 bool operator> (const T& x, const T& y) {return x.cmp(y) > 0; }

 template<class T>
 bool operator>=(const T& x, const T& y) {return x.cmp(y) >= 0; }

 template<class T>
 bool operator<=(const T& x, const T& y) {return x.cmp(y) <= 0; }

 template<class T>
 bool operator!=(const T& x, const T& y) {return x.cmp(y) != 0; }
