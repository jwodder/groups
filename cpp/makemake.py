#!/usr/bin/python
from __future__ import with_statement
import os
import os.path
import re
import sys

class Relation(object):
    # Used for calculating the transitive closures of the "#includes" and
    # "links with" relations
    # TODO: Replace this with the Floyd-Warshall algorithm?

    def __init__(self):
	self.relate = {}
	self.inverse = {}

    def add(self, x, y):
	self.relate.setdefault(x, set()).add(y)
	self.inverse.setdefault(y, set()).add(x)

    def transClosure(self):
	for x in set(self.relate) | set(self.inverse):
	    for a in self.inverse.get(x, set()):
		for b in self.relate.get(x, set()):
		    self.add(a,b)
	return self.relate


def toObject(fname):
    return os.path.splitext(fname)[0] + '.o'

if sys.path[0]: os.chdir(sys.path[0])

inclusions = Relation()
cpp = []
tests = []
for dirpath, dirnames, files in os.walk('.'):
    try:
	dirnames.remove('dev')
    except ValueError:
	pass
    for fname in files:
	if not (fname.endswith('.cpp') or fname.endswith('.hpp')):
	    continue
	fname = os.path.normpath(os.path.join(dirpath, fname))
	if os.path.normpath(dirpath) == 'tests':
	    tests.append(fname)
	elif fname.endswith('.cpp'):
	    cpp.append(fname)
	with open(fname) as fp:
	    for line in fp:
		m = re.search(r'^#include "([^"]+)"', line)
		if m:
		    inclusions.add(fname, m.group(1))

including = inclusions.transClosure()

objects = set(map(toObject, cpp))

print 'CXX = c++'
print 'CPPFLAGS = -std=c++98 -Wall -I.'
print 'CC = $(CXX)  # for linking'
print
print 'TESTS = ' + ' '.join(map(lambda t: os.path.splitext(t)[0], tests))
print
print 'all : ' + ' '.join(objects) + ' $(TESTS)'
print

for c in cpp:
    if including.get(c):
	print toObject(c) + ' : ' + ' '.join(including[c])
print

# If we are linking with a `foo.o` compiled from `foo.cpp`, and `foo.cpp`
# includes `bar.hpp`, and `bar.cpp` exists, then we also need to link with
# `bar.o`.
linksWith = Relation()
for fname in cpp + tests:
    oname = toObject(fname)
    for hpp in including.get(fname, []):
	obj = toObject(hpp)
	if obj in objects:
	    linksWith.add(oname, obj)
linking = linksWith.transClosure()

for t in tests:
    if linking.get(toObject(t)):
	print os.path.splitext(t)[0] + ' : ' + ' '.join(linking[toObject(t)])
    if including.get(t):
	print toObject(t) + ' : ' + ' '.join(including[t])
	print

print 'clean :'
print '\trm -f *.o Groups/*.o Groups/*/*.o tests/*.o'
print '\trm -f $(TESTS)'
