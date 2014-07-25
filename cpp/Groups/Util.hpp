/* Utilities which should be available to the user of the library */

#ifndef GROUPS_UTIL_H
#define GROUPS_UTIL_H

#include <ostream>
#include <iomanip>  /* setw */
#include <ios>  /* left */
#include <stdexcept>  /* logic_error */
#include <string>
#include <vector>
#include "Groups/BasicGroup.hpp"

namespace Groups {
 class group_mismatch : public std::logic_error {
 public:
  explicit group_mismatch(const std::string& m) : logic_error(m) { }
 };

 template<class T>
 std::ostream& showTable(std::ostream& out, const basic_group<T>& g) {
  std::vector<T> elems = g.elements();
  typename std::vector<T>::const_iterator xiter, yiter;
  size_t maxLen = 0;
  for (xiter = elems.begin(); xiter != elems.end(); xiter++) {
   size_t len = g.showElem(*xiter).size();
   if (len > maxLen) maxLen = len;
  }
  out << std::left;
  out << std::string(maxLen, ' ');
  for (yiter = elems.begin(); yiter != elems.end(); yiter++) {
   out << '|' << std::setw(maxLen) << g.showElem(*yiter);
  }
  out << std::endl;
  std::string tmp = std::string(maxLen, '-');
  out << tmp;
  tmp = "|" + tmp;
  for (int i=0; i<g.order(); i++) out << tmp;
  out << std::endl;
  for (xiter = elems.begin(); xiter != elems.end(); xiter++) {
   out << std::setw(maxLen) << g.showElem(*xiter);
   for (yiter = elems.begin(); yiter != elems.end(); yiter++) {
    out << '|' << std::setw(maxLen) << g.showElem(g.oper(*xiter, *yiter));
   }
   out << std::endl;
  }
  return out;
 }
}

#endif
