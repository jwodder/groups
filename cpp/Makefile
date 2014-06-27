CXX = c++
CPPFLAGS = -std=c++98 -Wall -I.
CC = $(CXX)  # for linking

TESTS = tests/closure01 tests/closure02 tests/closure03 tests/fromCycles01 tests/lehmer02 tests/lehmer03 tests/perminc01

all : Groups/Families/Dihedral.o Groups/Families/Symmetric.o Groups/Ops.o Groups/Families/Dicyclic.o Permutation.o Groups/Families/Tabular.o Groups/Families/Cyclic.o Groups/util.o Groups/Element.o $(TESTS)

Permutation.o : Permutation.hpp Groups/util.hpp
Groups/Element.o : Groups/Element.hpp Groups/Group.hpp
Groups/Ops.o : Groups/Families/Dicyclic.hpp Groups/Ops.hpp closure.hpp Groups/BasicGroup.hpp Groups/Element.hpp
Groups/util.o : Groups/util.hpp
Groups/Families/Cyclic.o : Groups/util.hpp Groups/Families/Cyclic.hpp Groups/BasicGroup.hpp closure.hpp
Groups/Families/Dicyclic.o : Groups/Families/Dicyclic.hpp Groups/util.hpp Groups/BasicGroup.hpp closure.hpp
Groups/Families/Dihedral.o : Groups/Families/Dihedral.hpp Groups/util.hpp Groups/BasicGroup.hpp closure.hpp
Groups/Families/Symmetric.o : Groups/Families/Symmetric.hpp Permutation.hpp Groups/util.hpp Groups/BasicGroup.hpp closure.hpp
Groups/Families/Tabular.o : Groups/Families/Tabular.hpp Groups/util.hpp Groups/BasicGroup.hpp closure.hpp

tests/closure01 : Permutation.o Groups/util.o
tests/closure01.o : Permutation.hpp closure.hpp

tests/closure02 : Groups/Families/Dicyclic.o Groups/Ops.o Groups/Element.o Groups/Families/Cyclic.o Groups/util.o
tests/closure02.o : Groups/Families/Cyclic.hpp Groups/Families/Dicyclic.hpp Groups/Ops.hpp closure.hpp Groups/Element.hpp Groups/BasicGroup.hpp

tests/closure03 : Groups/Families/Cyclic.o Groups/util.o
tests/closure03.o : Groups/Families/Direct.hpp closure.hpp Groups/Families/Cyclic.hpp Groups/BasicGroup.hpp Groups/util.hpp

tests/fromCycles01 : Permutation.o Groups/util.o
tests/fromCycles01.o : Permutation.hpp

tests/lehmer02 : Permutation.o Groups/util.o
tests/lehmer02.o : Permutation.hpp

tests/lehmer03 : Permutation.o Groups/util.o
tests/lehmer03.o : Permutation.hpp

tests/perminc01 : Permutation.o Groups/util.o
tests/perminc01.o : Permutation.hpp

clean :
	rm -f *.o Groups/*.o Groups/*/*.o tests/*.o
	rm -f $(TESTS)
