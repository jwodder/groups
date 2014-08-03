import sys
sys.path.insert(1, sys.path[0] + '/..')
import groups
from groups.about import about

g = groups.Symmetric(4)
data = about(g)
for subgr in sorted(data["subgroups"], key=lambda s: data["subgroups"][s]["index"]):
    print '{' + ', '.join(data["subgroups"][subgr]["elements"]) + '}'
    for gen in data["subgroups"][subgr]["generators"]:
	print ' {' + ', '.join(gen) + '}'
    print
