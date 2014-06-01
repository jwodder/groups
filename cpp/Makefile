CXX = c++
CPPFLAGS = -std=c++98 -Wall -I.
CC = $(CXX)  # for linking

GROUPS = Cyclic.o Dicyclic.o Dihedral.o Direct.o Symmetric.o Tabular.o

OBJS = $(GROUPS) Element.o Grutils.o Permutation.o util.o

TESTS = tests/closure01 tests/closure02 tests/fromCycles01 tests/lehmer02 \
 tests/lehmer03 tests/perminc01

all : $(OBJS) $(TESTS)

$(GROUPS) : util.hpp Group.hpp Element.hpp

Cyclic.o : Cyclic.hpp
Dicyclic.o : Dicyclic.hpp
Dihedral.o : Dihedral.hpp
Direct.o : Direct.hpp
Symmetric.o : Symmetric.hpp Permutation.hpp
Tabular.o : Tabular.hpp

Element.o : Element.hpp Group.hpp
Grutils.o : Grutils.hpp Element.hpp closure.hpp Dicyclic.hpp Group.hpp
Permutation.o : Permutation.hpp util.hpp
util.o : util.hpp

tests/closure01 : Permutation.o util.o
tests/closure01.o : closure.hpp Permutation.hpp

tests/closure02 : Cyclic.o Grutils.o Element.o util.o Dicyclic.o
tests/closure02.o : Cyclic.hpp Grutils.hpp Group.hpp Element.hpp Dicyclic.hpp closure.hpp

tests/fromCycles01 : Permutation.o util.o
tests/fromCycles01.o : Permutation.hpp

tests/lehmer02 : Permutation.o util.o
tests/lehmer02.o : Permutation.hpp

tests/lehmer03 : Permutation.o util.o
tests/lehmer03.o : Permutation.hpp

tests/perminc01 : Permutation.o util.o
tests/perminc01.o : Permutation.hpp

clean :
	rm -f *.o tests/*.o $(TESTS)
