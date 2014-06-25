CXX = c++
CPPFLAGS = -std=c++98 -Wall -I.
CC = $(CXX)  # for linking

GRNAMES = Cyclic Dicyclic Dihedral Symmetric Tabular
GROUPS = $(foreach grp,$(GRNAMES),Groups/Families/$(grp).o)

OBJS = $(GROUPS) Groups/Element.o Groups/Ops.o Permutation.o Groups/util.o

TESTS = tests/closure01 tests/closure02 tests/fromCycles01 tests/lehmer02 \
 tests/lehmer03 tests/perminc01

all : $(OBJS) $(TESTS)

$(GROUPS) : Groups/util.hpp Groups/Group.hpp Groups/Element.hpp

$(foreach grp,$(GROUPS),$(grp)) : Groups/Families/%.o : Groups/Families/%.hpp
Groups/Families/Symmetric.o : Permutation.hpp

Groups/Element.o : Groups/Element.hpp Groups/Group.hpp
Groups/Ops.o : Groups/Ops.hpp Groups/Element.hpp closure.hpp \
 Groups/Families/Dicyclic.hpp Groups/Group.hpp
Groups/util.o : Groups/util.hpp
Permutation.o : Permutation.hpp Groups/util.hpp

tests/closure01 : Permutation.o Groups/util.o
tests/closure01.o : closure.hpp Permutation.hpp

tests/closure02 : Groups/Families/Cyclic.o Groups/Ops.o Groups/Element.o \
 Groups/util.o Groups/Families/Dicyclic.o
tests/closure02.o : Groups/Families/Cyclic.hpp Groups/Ops.hpp \
 Groups/Group.hpp Groups/Element.hpp Groups/Families/Dicyclic.hpp closure.hpp

tests/closure03 : Groups/Families/Cyclic.o Groups/util.o
tests/closure03.o : Groups/Families/Cyclic.hpp Groups/Group.hpp closure.hpp

tests/fromCycles01 : Permutation.o Groups/util.o
tests/fromCycles01.o : Permutation.hpp

tests/lehmer02 : Permutation.o Groups/util.o
tests/lehmer02.o : Permutation.hpp

tests/lehmer03 : Permutation.o Groups/util.o
tests/lehmer03.o : Permutation.hpp

tests/perminc01 : Permutation.o Groups/util.o
tests/perminc01.o : Permutation.hpp

clean :
	rm -f *.o Groups/*.o Groups/Families/*.o tests/*.o
	rm -f $(TESTS)
