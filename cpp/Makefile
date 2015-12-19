CXX = c++
CPPFLAGS = -std=c++11 -Wall -pedantic -I.
CC = $(CXX)  # for linking

TESTS = tests/closure01 tests/closure03 tests/conj01 tests/direct01 tests/direct02 tests/fromCycles01 tests/indices01 tests/indices02 tests/indices03 tests/indices04 tests/indices05 tests/indices06 tests/indices07 tests/indices08 tests/indices09 tests/lehmer02 tests/lehmer03 tests/perminc01 tests/readCycles01 tests/subgrgens

all : Groups/internals.o Groups/Families/Dihedral.o Groups/Families/Klein4.o Groups/Families/Symmetric.o Groups/Families/Dicyclic.o Permutation.o Groups/Families/Cyclic.o Groups/Families/AutCyclic.o Groups/Group.o Groups/Element.o $(TESTS)

Permutation.o : Permutation.hpp Groups/internals.hpp
Groups/Element.o : Groups/Element.hpp closure.hpp Groups/BasicGroup.hpp Groups/Group.hpp Groups/internals.hpp
Groups/Group.o : Groups/Util.hpp Groups/Families/Cyclic.hpp Groups/Group.hpp Groups/Families/Dicyclic.hpp Groups/internals.hpp closure.hpp Groups/Element.hpp Groups/Families/Trivial.hpp Groups/Families/AutCyclic.hpp Permutation.hpp Groups/Families/Symmetric.hpp Groups/Families/Dihedral.hpp Groups/Families/Klein4.hpp Groups/BasicGroup.hpp
Groups/internals.o : Groups/internals.hpp
Groups/Families/AutCyclic.o : Groups/internals.hpp Groups/Util.hpp Groups/BasicGroup.hpp closure.hpp Groups/Families/AutCyclic.hpp
Groups/Families/Cyclic.o : Groups/internals.hpp Groups/Util.hpp Groups/Families/Cyclic.hpp Groups/BasicGroup.hpp closure.hpp
Groups/Families/Dicyclic.o : Groups/Families/Dicyclic.hpp Groups/internals.hpp Groups/Util.hpp Groups/BasicGroup.hpp closure.hpp
Groups/Families/Dihedral.o : Groups/Families/Dihedral.hpp Groups/Util.hpp Groups/BasicGroup.hpp closure.hpp Groups/internals.hpp
Groups/Families/Klein4.o : Groups/internals.hpp closure.hpp Groups/Families/Klein4.hpp Groups/BasicGroup.hpp
Groups/Families/Symmetric.o : Groups/Util.hpp Groups/internals.hpp closure.hpp Groups/Families/Symmetric.hpp Permutation.hpp Groups/BasicGroup.hpp

tests/closure01 : Groups/internals.o Permutation.o
tests/closure01.o : Permutation.hpp closure.hpp

tests/closure03 : Groups/internals.o Groups/Families/Cyclic.o
tests/closure03.o : Groups/Util.hpp Groups/Families/Cyclic.hpp Groups/internals.hpp closure.hpp Groups/Families/Direct.hpp Groups/BasicGroup.hpp

tests/conj01 : Groups/internals.o Groups/Families/Symmetric.o Groups/Families/Klein4.o Groups/Families/Dicyclic.o Permutation.o Groups/Families/Cyclic.o Groups/Group.o Groups/Families/Dihedral.o Groups/Families/AutCyclic.o Groups/Element.o
tests/conj01.o : Groups/Group.hpp Groups/Families/Dicyclic.hpp Groups/internals.hpp closure.hpp Groups/Element.hpp Groups/BasicGroup.hpp

tests/direct01 : Groups/internals.o Groups/Families/Symmetric.o Groups/Families/Klein4.o Groups/Families/Dicyclic.o Permutation.o Groups/Families/Cyclic.o Groups/Group.o Groups/Families/Dihedral.o Groups/Families/AutCyclic.o Groups/Element.o
tests/direct01.o : Groups/Util.hpp Groups/Families/Cyclic.hpp Groups/Group.hpp Groups/Families/Dicyclic.hpp Groups/internals.hpp closure.hpp Groups/Element.hpp Groups/BasicGroup.hpp

tests/direct02 : Groups/internals.o Groups/Families/Symmetric.o Groups/Families/Klein4.o Groups/Families/Dicyclic.o Permutation.o Groups/Families/Cyclic.o Groups/Group.o Groups/Families/Dihedral.o Groups/Families/AutCyclic.o Groups/Element.o
tests/direct02.o : Groups/Util.hpp Groups/Families/Cyclic.hpp Groups/Group.hpp Groups/Families/Dicyclic.hpp Groups/internals.hpp closure.hpp Groups/Element.hpp Groups/Families/Direct.hpp Groups/BasicGroup.hpp

tests/fromCycles01 : Groups/internals.o Permutation.o
tests/fromCycles01.o : Permutation.hpp

tests/indices01 : Groups/internals.o Groups/Families/Cyclic.o
tests/indices01.o : Groups/internals.hpp closure.hpp Groups/Families/Cyclic.hpp Groups/BasicGroup.hpp

tests/indices02 : Groups/internals.o Groups/Families/Dicyclic.o
tests/indices02.o : Groups/Families/Dicyclic.hpp Groups/internals.hpp closure.hpp Groups/BasicGroup.hpp

tests/indices03 : Groups/Families/Dihedral.o Groups/internals.o
tests/indices03.o : Groups/Families/Dihedral.hpp closure.hpp Groups/BasicGroup.hpp Groups/internals.hpp

tests/indices04 : Groups/internals.o Groups/Families/Dicyclic.o Groups/Families/Cyclic.o
tests/indices04.o : Groups/Util.hpp Groups/Families/Cyclic.hpp Groups/Families/Dicyclic.hpp Groups/internals.hpp closure.hpp Groups/Families/Direct.hpp Groups/BasicGroup.hpp

tests/indices05 : Groups/internals.o Groups/Families/Symmetric.o Permutation.o
tests/indices05.o : Groups/Families/Symmetric.hpp Groups/internals.hpp closure.hpp Groups/BasicGroup.hpp Permutation.hpp

tests/indices06 : Groups/internals.o Groups/Families/Symmetric.o Groups/Families/Klein4.o Groups/Families/Dicyclic.o Permutation.o Groups/Families/Cyclic.o Groups/Group.o Groups/Families/Dihedral.o Groups/Families/AutCyclic.o Groups/Element.o
tests/indices06.o : Groups/Util.hpp Groups/Families/Cyclic.hpp Groups/Group.hpp Groups/Families/Dicyclic.hpp Groups/internals.hpp closure.hpp Groups/Element.hpp Groups/Families/Direct.hpp Groups/BasicGroup.hpp

tests/indices07 : Groups/internals.o Groups/Families/AutCyclic.o
tests/indices07.o : Groups/internals.hpp closure.hpp Groups/BasicGroup.hpp Groups/Families/AutCyclic.hpp

tests/indices08 : Groups/internals.o Groups/Families/Klein4.o
tests/indices08.o : Groups/internals.hpp closure.hpp Groups/Families/Klein4.hpp Groups/BasicGroup.hpp

tests/indices09 : Groups/internals.o
tests/indices09.o : Groups/internals.hpp closure.hpp Groups/BasicGroup.hpp Groups/Families/Trivial.hpp

tests/lehmer02 : Groups/internals.o Permutation.o
tests/lehmer02.o : Permutation.hpp

tests/lehmer03 : Groups/internals.o Permutation.o
tests/lehmer03.o : Permutation.hpp

tests/perminc01 : Groups/internals.o Permutation.o
tests/perminc01.o : Permutation.hpp

tests/readCycles01 : Groups/internals.o Permutation.o
tests/readCycles01.o : Permutation.hpp

tests/subgrgens : Groups/internals.o Groups/Families/Symmetric.o Permutation.o
tests/subgrgens.o : Groups/Families/Symmetric.hpp Groups/internals.hpp closure.hpp Groups/BasicGroup.hpp Permutation.hpp

clean :
	rm -f *.o Groups/*.o Groups/*/*.o tests/*.o
	rm -f $(TESTS)
