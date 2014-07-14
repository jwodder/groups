/* Utilities which should be available to the user of the library */

#ifndef GROUPS_UTIL_H
#define GROUPS_UTIL_H

#include <stdexcept>  /* logic_error */
#include <string>

namespace Groups {
 class group_mismatch : public std::logic_error {
 public:
  explicit group_mismatch(const std::string& m) : logic_error(m) { }
 };
}

#endif
