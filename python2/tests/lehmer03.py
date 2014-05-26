import sys
sys.path.insert(1, sys.path[0] + '/..')
from permutation import Permutation

for i in range(2, 13):
    for j in range(1,i):
	p = Permutation.transposition(i,j)
	lehmer = p.lehmer
	remhel = Permutation.fromLehmer(lehmer)
	#if p != remhel:
	print '%s -> %d -> %s' % (p, lehmer, remhel)
