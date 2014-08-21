# -*- coding: utf-8 -*-
import re

subs   = {0x30: u'₀', 0x31: u'₁', 0x32: u'₂', 0x33: u'₃', 0x34: u'₄',
	  0x35: u'₅', 0x36: u'₆', 0x37: u'₇', 0x38: u'₈', 0x39: u'₉',
	  0x2d: u'₋'}

def sub(n):  n = str(n); return '_{' + n + '}' if len(n) > 1 else '_' + n
def subU(n): return unicode(n).translate(subs)

supers = {0x30: u'⁰', 0x31: u'¹', 0x32: u'²', 0x33: u'³', 0x34: u'⁴',
	  0x35: u'⁵', 0x36: u'⁶', 0x37: u'⁷', 0x38: u'⁸', 0x39: u'⁹',
	  0x2d: u'⁻'}

def sup(n):  n = str(n); return '^{' + n + '}' if len(n) > 1 else '^' + n
def supU(n): return unicode(n).translate(supers)

def uniexp(str):
    def mogrify(match):
	(cmd, val1, val2) = match.groups()
	return (val1 or val2).translate(subs if cmd == u'_' else supers)
    return re.sub(ur'([_^])(?:([-\d])|\{(-?\d*)\})', mogrify, unicode(str))

def multish(x,y):
    if x == '1': return y
    elif y == '1': return x
    else: return x + y

def shexp(x,i):
    if i == 0: return '1'
    elif i == 1: return x
    else: return x + sup(i)

def cycOrd(n,x): return n // gcd(x,n)

def parenth(s):
    """Parenthesizes a string if & only if it contains an unbracketed space"""
    lvl = 0
    for c in s:
	if c == ' ' and lvl == 0:
	    return '(' + s + ')'
	elif c in '([{':
	    lvl += 1
	elif c in '}])':
	    lvl -= 1
    return s

def showbinop(x,op,y):
    return parenth(str(x)) + ' ' + op + ' ' + parenth(str(y))

def showbinopU(x,op,y):
    return parenth(unicode(x)) + u' ' + op + u' ' + parenth(unicode(y))

def factorial(n):
    x=1
    for i in range(2,n+1):
	x *= i
    return x

def gcd(x,y):
    (a,b) = (abs(x), abs(y))
    if a == 0 and b == 0: return 0
    elif a == 0 or b == 0: return a or b
    while b != 0:
	(a,b) = (b, a % b)
    return a

def lcm(x,y): d = gcd(x,y); return 0 if d == 0 else abs(x*y) // d

def modInverse(a,n):
    (u, uc) = (abs(n), 0)
    (l, lc) = (a % u, 1)
    while l > 1:
	(u, uc, l, lc) = (l, lc, u % l, uc - lc * (u//l))
    if l == 1: return lc % abs(n)
    else: raise ValueError('%d has no multiplicative inverse modulo %d' % (a,n))

def isPair(x): return isinstance(x, tuple) and len(x) == 2
