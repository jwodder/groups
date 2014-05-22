from groups      import Symmetric
from permutation import Permutation

for x in Symmetric(4):
    lehmer = x.lehmer
    remhel = Permutation.fromLehmer(lehmer)
    if x != remhel:
	print '%s -> %d -> %s' % (x, lehmer, remhel)
