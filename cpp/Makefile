CXX = c++
CPPFLAGS = -std=c++98 -Wall -I.
CC = $(CXX)  # for linking

TESTS = tests/closure01 tests/closure03 tests/direct01 tests/fromCycles01 tests/indices01 tests/indices02 tests/indices03 tests/indices04 tests/indices05 tests/indices06 tests/lehmer02 tests/lehmer03 tests/perminc01 tests/readCycles01

all : Groups/Families/Dihedral.o Groups/Families/Symmetric.o Groups/Families/Dicyclic.o Permutation.o Groups/Families/Cyclic.o Groups/util.o Groups/Group.o Groups/Element.o $(TESTS)

Permutation.o : Permutation.hpp Groups/util.hpp
Groups/Element.o : Groups/Element.hpp Groups/util.hpp Groups/BasicGroup.hpp closure.hpp Groups/Group.hpp
Groups/Group.o : Groups/Element.hpp Groups/util.hpp Groups/BasicGroup.hpp closure.hpp Groups/Group.hpp
Groups/util.o : Groups/util.hpp
Groups/Families/Cyclic.o : Groups/util.hpp Groups/Families/Cyclic.hpp Groups/BasicGroup.hpp closure.hpp
Groups/Families/Dicyclic.o : Groups/Families/Dicyclic.hpp Groups/util.hpp Groups/BasicGroup.hpp closure.hpp
Groups/Families/Dihedral.o : Groups/Families/Dihedral.hpp Groups/util.hpp Groups/BasicGroup.hpp closure.hpp
Groups/Families/Symmetric.o : Groups/Families/Symmetric.hpp Permutation.hpp Groups/util.hpp Groups/BasicGroup.hpp closure.hpp

tests/closure01 : Permutation.o Groups/util.o
tests/closure01.o : Permutation.hpp closure.hpp

tests/closure03 : Groups/Families/Cyclic.o Groups/util.o
tests/closure03.o : Groups/Families/Direct.hpp closure.hpp Groups/Families/Cyclic.hpp Groups/BasicGroup.hpp Groups/util.hpp

tests/direct01 : Groups/Families/Dicyclic.o Groups/Group.o Groups/Element.o Groups/Families/Cyclic.o Groups/util.o
tests/direct01.o : Groups/util.hpp Groups/Families/Cyclic.hpp Groups/Group.hpp Groups/Families/Dicyclic.hpp closure.hpp Groups/Element.hpp Groups/BasicGroup.hpp

tests/fromCycles01 : Permutation.o Groups/util.o
tests/fromCycles01.o : Permutation.hpp

tests/indices01 : Groups/Families/Cyclic.o Groups/util.o
tests/indices01.o : closure.hpp Groups/Families/Cyclic.hpp Groups/BasicGroup.hpp

tests/indices02 : Groups/Families/Dicyclic.o Groups/util.o
tests/indices02.o : Groups/Families/Dicyclic.hpp closure.hpp Groups/BasicGroup.hpp

tests/indices03 : Groups/Families/Dihedral.o Groups/util.o
tests/indices03.o : Groups/Families/Dihedral.hpp closure.hpp Groups/BasicGroup.hpp

tests/indices04 : Groups/Families/Dicyclic.o Groups/Families/Cyclic.o Groups/util.o
tests/indices04.o : Groups/util.hpp Groups/Families/Cyclic.hpp Groups/Families/Dicyclic.hpp closure.hpp Groups/Families/Direct.hpp Groups/BasicGroup.hpp

tests/indices05 : Groups/Families/Symmetric.o Permutation.o Groups/util.o
tests/indices05.o : Groups/Families/Symmetric.hpp Permutation.hpp closure.hpp Groups/BasicGroup.hpp

tests/indices06 : Groups/Families/Dicyclic.o Groups/Group.o Groups/Element.o Groups/Families/Cyclic.o Groups/util.o
tests/indices06.o : Groups/util.hpp Groups/Families/Cyclic.hpp Groups/Group.hpp Groups/Families/Dicyclic.hpp closure.hpp Groups/Element.hpp Groups/Families/Direct.hpp Groups/BasicGroup.hpp

tests/lehmer02 : Permutation.o Groups/util.o
tests/lehmer02.o : Permutation.hpp

tests/lehmer03 : Permutation.o Groups/util.o
tests/lehmer03.o : Permutation.hpp

tests/perminc01 : Permutation.o Groups/util.o
tests/perminc01.o : Permutation.hpp

tests/readCycles01 : Permutation.o Groups/util.o
tests/readCycles01.o : Permutation.hpp

clean :
	rm -f *.o Groups/*.o Groups/*/*.o tests/*.o
	rm -f $(TESTS)
