HC = ghc
HC_OPTS = -O -Wall
.SUFFIXES : .o .hs .hi .hc

OBJS = Closure.hi Permutation.hi Groups.hi Groups/Type.hi \
 Groups/Families.hi Groups/Ops.hi Groups/Subgroups.hi Groups/Internals.hi

all : $(OBJS)

.o.hi : ;
.hs.o :
	$(HC) -c $< $(HC_OPTS)

Groups.o : Groups/Families.o Groups/Ops.o Groups/Type.o
Groups/Families.o Groups/Ops.o : Groups/Type.o Groups/Internals.o Closure.o
Groups/Families.o : Permutation.o
Groups/Subgroups.o : Groups/Type.o Groups/Ops.o
Groups/Internals.o : Groups/Type.o Closure.o

doc : Closure.hs Permutation.hs Groups.hs Groups/*.hs
	haddock -ho doc -t Groups $^

clean :
	( cd Groups; rm -f *.o *.hi )
	rm -f *.o *.hi
	rm -rf doc
