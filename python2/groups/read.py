# -*- coding: utf-8 -*-
from . import types as G

__all__ = ["readName"]

def readName(s): return GroupReader().read(s)

class GroupReaderError(ValueError): pass

tokens = {"Dih": ('Dih', r'\Dih', r'\operatorname{Dih}', 'D'),
	  "Dic": ('Dic', r'\Dic', r'\operatorname{Dic}'),
	  "Z": ('Z', r'\Z', 'ℤ', 'C', r'\mathbb{Z}'),
	  "S": ('S',),
	  "A": ('A',),
	  "Q": ('Q',),
	  "V_4": ('V_4', 'V_{4}', 'V'),
	  '×': ('times', r'\times', r'\times{}', '×', 'x', 'X', '*'),
	  '^×': ('^rtimes', r'^\rtimes', r'^\rtimes{}', r'^{\rtimes}',
		 '^×', '^x', '^X', '^*', '^ˣ', 'ˣ'),
	 #'⋊': ('rtimes', r'\rtimes', r'\rtimes{}', 'x|'),
	  '_': ('_',),
	  '(': ('(',),
	  ')': (')',),
	 }

tokenTrie = dict()
for (token, lexemes) in tokens.iteritems():
    for lxm in lexemes:
	current = tokenTrie
	for c in lxm:
	    current = current.setdefault(c, dict())
	current[None] = token

def lex(s):
    i = 0
    while i < len(s):
	if s[i].isspace():
	    i += 1
	elif s[i] in tokenTrie:
	    j = i+1
	    stack = [tokenTrie[s[i]]]
	    while j < len(s) and s[j] in stack[-1]:
		stack.append(stack[-1][s[j]])
		j += 1
	    while stack and None not in stack[-1]:
		stack.pop()
		j -= 1
	    if stack:
		yield stack[-1][None]
		i = j
	    else:
		raise GroupReaderError('Invalid lexeme: %r'
				       % (s[i:].split()[0],))
	elif s[i].isdigit():
	    j = i+1
	    while j < len(s) and s[j].isdigit(): j += 1
	    yield int(s[i:j])
	    i = j
	elif s[i] == '{':
	    i += 1
	    j = i
	    while j < len(s) and s[j].isdigit(): j += 1
	    if i == j or j == len(s) or s[j] != '}':
		raise GroupReaderError("'{' must be followed by a natural"
				       " number and '}'")
	    yield int(s[i:j])
	    i = j+1
	else:
	    raise GroupReaderError('Invalid lexeme: %r' % (s[i:].split()[0],))

subscripted = {"Dih": G.Dihedral,
	       "Dic": G.Dicyclic,
	       "Z":   G.Cyclic,
	       "S":   G.Symmetric,
	       "A":   G.Alternating,
	       "Q":   lambda n: G.Quaternion(order=n)}

class GroupReader(object):
    def __init__(self):
	self.state = self.beforeGroup
	self.stack = [None]

    def read(self, s):
	for t in lex(s):
	    self.state = self.state(t)
	if self.state not in (self.afterGroup, self.afterCyclic):
	    raise GroupReaderError('Input ended in middle of parse')
	elif len(self.stack) > 1:
	    raise GroupReaderError('Unclosed parentheses')
	if self.state == self.afterCyclic:  # `is` doesn't work here.
	    self.pushGroup(self.tmp)
	return self.stack[-1]

    def pushGroup(self, g):
	if self.stack[-1] is None:
	    self.stack[-1] = g
	else:
	    self.stack[-1] = G.Direct(self.stack[-1], g)

    def beforeGroup(self, t):  # The next token sequence must be a group or (
	if t == '(':
	    self.stack.append(None)
	    return self.beforeGroup
	elif t == 'V_4':
	    self.pushGroup(G.Klein4())
	    return self.afterGroup
	elif t == 1:
	    self.pushGroup(G.Trivial())
	    return self.afterGroup
	elif t in subscripted:
	    self.classToken = t
	    return self.expectUnderscore
	else:
	    raise GroupReaderError('Expected group, got %r' % (t,))

    def expectUnderscore(self, t):
	if t == '_':
	    return self.expectSubscript
	else:
	    raise GroupReaderError('Underscore expected after %r' 
				    % (self.classToken,))

    def expectSubscript(self, t):
	if isinstance(t, (int, long)):
	    self.tmp = subscripted[self.classToken](t)
	    if self.classToken == 'Z':
		return self.afterCyclic
	    else:
		self.pushGroup(self.tmp)
		return self.afterGroup
	else:
	    raise GroupReaderError('Number expected after underscore')

    def afterGroup(self, t):  # The next token must be × or )
	if t == '×':
	    return self.beforeGroup
	elif t == ')':
	    g = self.stack.pop()
	    if self.stack:
		self.pushGroup(g)
		return self.afterGroup
	    else:
		raise GroupReaderError('Too many closing parentheses')
	else:
	    raise GroupReaderError('×, ), or end of input required after group')

    def afterCyclic(self, t):  # The next token must be ^×, ×, or )
	if t == '^×':
	    self.pushGroup(G.AutCyclic(self.tmp.n))
	    return self.afterGroup
	else:
	    self.pushGroup(self.tmp)
	    return self.afterGroup(t)
