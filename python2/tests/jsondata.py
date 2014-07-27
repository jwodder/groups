#!/usr/bin/python
import sys
sys.path.insert(1, sys.path[0] + '/..')
import groups
from groups.internals import gcd

#g = groups.Quaternion()
#g = groups.Alternating(4)
g = groups.Cyclic(17)

def printf(format, *args): sys.stdout.write(format % args)

def jsonify(obj):
    if obj is None: return 'null'
    elif isinstance(obj, bool): return repr(obj).lower()
    elif isinstance(obj, (list, tuple)):
	return '[' + ', '.join(map(jsonify, obj)) + ']'
    elif isinstance(obj, dict):
	return '{' + ', '.join(jsonify(k) + ': ' + jsonify(obj[k])
			       for k in sorted(obj.keys())) + '}'
    elif isinstance(obj, (int, long)): return str(obj)
    else:
	obj = obj.replace("\\", "\\\\")
	obj = obj.replace('"', '\\"')
	obj = obj.replace("\r", "\\n")
	obj = obj.replace("\n", "\\n")
	obj = obj.replace("\t", "\\t")
	return '"' + obj + '"'

def set2list(s): return [g.showElem(x) for x in sorted(s)]

subgrGens   = g.subgroupGens()
subgrKeys   = dict((h, tuple(sorted(h))) for h in subgrGens.iterkeys())
subgrSorted = sorted(subgrKeys, key=lambda h: subgrKeys[h])
subgrNames  = dict((h, 'subgr%02d' % (i,)) for (i,h) in enumerate(subgrSorted))

def nameSet(s): return subgrNames[frozenset(s)]

printf('{\n')
printf(' "name": %s,\n', jsonify(str(g)))
printf(' "order": %d,\n', len(g))
printf(' "abelian": %s,\n', jsonify(g.isAbelian()))
printf(' "exponent": %d,\n', g.exponent())
printf(' "rank": %d,\n', min(len(gen) for gen in subgrGens[frozenset(g)]))
printf(' "center": %s,\n', jsonify(nameSet(g.center())))

printf(' "elements": {')
first = True
for (i,x) in enumerate(g):
    if first:
	first = False
    else:
	printf(',')
    printf('\n  %s: %s', jsonify(g.showElem(x)),
			 jsonify({"index":   i,
				  "order":   g.order(x),
				  "inverse": g.showElem(g.invert(x)),
				  "cycle":   nameSet(g.cycle(x)),
				  "centralizer": nameSet(g.centralizer([x])),
				 }))
printf('\n },\n')

printf(' "subgroups": {')
first = True
for (i,h) in enumerate(subgrSorted):
    if first:
	first = False
    else:
	printf(',')
    printf('\n  %s: %s', jsonify(subgrNames[h]),
			 jsonify({"index":    i,
				  "elements": map(g.showElem, subgrKeys[h]),
				  "normal":   g.isNormal(h),
				  "order":    len(h),
				  "centralizer": nameSet(g.centralizer(h)),
				  "normalizer": nameSet(g.normalizer(h)),
				  "generators": [map(g.showElem, gen) for gen in sorted(tuple(sorted(gen)) for gen in subgrGens[h])],
				 }))
printf('\n },\n')

printf(' "conjugacy_classes": [')
first = True
for cclass in g.conjugacies():
    if first:
	first = False
    else:
	printf(',')
    printf('\n  %s', jsonify(set2list(cclass)))
printf('\n ],\n')

lc = []
for h in g.lowerCentral():
    lc.append(nameSet(h))
    if len(h) == 1:
	nilpotence = len(lc) - 1
	break
    elif len(lc) > 1 and lc[-1] == lc[-2]:
	nilpotence = None
	break
printf(' "nilpotence": %s,\n', jsonify(nilpotence))
printf(' "lower_central_series": %s,\n', jsonify(lc))
printf(' "commutator_subgroup": %s,\n', jsonify(lc[1] if len(lc)>1 else lc[0]))

if nilpotence is not None:
    printf(' "solvable": true,\n')
else:
    subQtys = set()
    for h in subgrGens:
	subQtys.add(len(h))
    for i in range(2, len(g)):
	if len(g) % i == 0 and gcd(i, len(g) // i) == 1 and i not in subQtys:
	    printf(' "solvable": false,\n')
	    break
    else:
	printf(' "solvable": true,\n')

printf(' "simple": %s\n', jsonify(len(g) != 1 and not any(g.isNormal(h) for h in subgrGens if len(h) not in (1, len(g)))))

#commutators?
#automorphisms that elements induce by conjugation?
#maximal subgroups?
#maximal subgroups of each subgroup?
#cosets of subgroups?
#abelianity of subgroups?
#normal subgroups within subgroups?

printf('}\n')
