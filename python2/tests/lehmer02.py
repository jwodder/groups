import sys
sys.path.insert(1, sys.path[0] + '/..')
from groups      import Symmetric
from permutation import Permutation

for x in Symmetric(5):
    lehmer = x.lehmer
    remhel = Permutation.fromLehmer(lehmer)
    if x != remhel:
	print '%s -> %d -> %s' % (x, lehmer, remhel)
