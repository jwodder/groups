#!/usr/bin/python
# -*- coding: utf-8 -*-
from sys import stdout
import groups

#group = groups.Cyclic(5)
#group = groups.Cyclic(3) * groups.Cyclic(2)
#group = groups.Dicyclic(2)
#group = groups.Dicyclic(2) * groups.Cyclic(2)  # Q_8 × Z_2
group = groups.Symmetric(3)

def wprint(*args):
    for obj in args:
	if isinstance(obj, str): stdout.write(obj)
	elif isinstance(obj, unicode): stdout.write(obj.encode('utf-8'))
	else: stdout.write(repr(obj))

elems = group.elements()
fwidth = max(len(unicode(x)) for x in elems)
wprint(' ' * fwidth)
for y in elems:
    wprint(u'|%-*s' % (fwidth, y))
wprint('\n')
wprint('-' * fwidth, ('|' + '-' * fwidth) * len(group), '\n')
for x in elems:
    wprint(u'%-*s' % (fwidth, x))
    for y in elems:
	wprint(u'|%-*s' % (fwidth, x*y))
    wprint('\n')
