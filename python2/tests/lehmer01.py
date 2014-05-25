import sys
sys.path.insert(1, sys.path[0] + '/..')
from groups import Symmetric

for x in Symmetric(5): print '%d\t%s' % (x.lehmer, x)
