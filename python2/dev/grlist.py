from types import *

__all__ = ["groupList"]

def elemAbel(p,n):
    return reduce(DirectProduct, [Cyclic(p)] * n)
    ### TODO: Set the name of the group to `sub('E', p**n)`

QD16 = CycSemiCyc(8,2,3)  ### Change name to "QD_{16}"

M16 = CycSemiCyc(8,2,5)  ### Change name to "M"

Q8sZ2 = Semidirect(Quaternion(2), Cyclic(2),
		   lambda x:
		     lambda (i,j):
		       ((i + 2*(i%2) + 2*j) % 4 if x else i, j))

V4sZ4 = Semidirect(Klein4(), Cyclic(4),
		   lambda x: lambda (a,b): (b,a) if x%2 else (a,b))

o32nonA = [
    Quaternion(4),
    Dihedral(16),
    CycSemiCyc(16,2,7),
    direct(Quaternion(3), Cyclic(2)),
    direct(QD16, Cyclic(2)),
    direct(Dihedral(8), Cyclic(2)),
    CycSemiCyc(8,4,-1),
    CycSemiCyc(8,4,3),
    HolCyclic(8),
    direct(M16, Cyclic(2)),
    direct(Quaternion(2), Klein4()),
    direct(Q8sZ2, Cyclic(2)),
    direct(Dihedral(4), Klein4()),
    direct(CycSemiCyc(4,4,-1), Cyclic(2)),
    direct(V4sZ4, Cyclic(2)),
    direct(Quaternion(2), Cyclic(4)),
    direct(Dihedral(4), Cyclic(4)),
    CycSemiCyc(8,4,5),
    CycSemiCyc(16,2,9),
    GDih(direct(Cyclic(4), Cyclic(4))),
    ### Q_8\rtimes\Z_4
    ### Q_8\rtimes V_4
]

groupList = [
# Should the elements of this list be `namedtuple`s with "abelian" and
# "nonabelian" fields?
    ([Trivial()], []),
    ([Cyclic(2)], []),
    ([Cyclic(3)], []),
    ([Cyclic(4), Klein4()], []),
    ([Cyclic(5)], []),
    ([Cyclic(6)], [Symmetric(3)]),
    ([Cyclic(7)], []),
    ([Cyclic(8), direct(Cyclic(4), Cyclic(2)), elemAbel(2,3)],
     [Quaternion(2), Dihedral(4)]),
    ([Cyclic(9), elemAbel(3,2)], []),
    ([Cyclic(10)], [Dihedral(5)]),
    ([Cyclic(11)], []),
    ([Cyclic(12), direct(Cyclic(6), Cyclic(2))],
     [Alternating(4), Dihedral(6), Dicyclic(3)]),
    ([Cyclic(13)], []),
    ([Cyclic(14)], [Dihedral(7)]),
    ([Cyclic(15)], []),
    ([Cyclic(16), direct(Cyclic(8), Cyclic(2)), direct(Cyclic(4), Cyclic(4)),
      direct(Cyclic(4), Klein4()), elemAbel(2,4)],
     [Quaternion(3), QD16, M16, direct(Quaternion(2), Cyclic(2)), Q8sZ2,
      direct(Dihedral(4), Cyclic(2)), CycSemiCyc(4,4,-1), V4sZ4, Dihedral(8)]),
    ([Cyclic(17)], []),
    ([Cyclic(18), direct(Cyclic(6), Cyclic(3))],
     [Dihedral(9), direct(Symmetric(3), Cyclic(3)), GDih(elemAbel(3,2))]),
    ([Cyclic(19)], []),
    ([Cyclic(20), direct(Cyclic(10), Cyclic(2))],
     [CycSemiCyc(5,4,3),  ### TODO: Set name to "F_{20}"
      Dicyclic(5), Dihedral(10)]),
    ([Cyclic(21)], [CycSemiCyc(7,3,2)]),
    ([Cyclic(22)], [Dihedral(11)]),
    ([Cyclic(23)], []),

    ([Cyclic(24), direct(Cyclic(12), Cyclic(2)), direct(Cyclic(6), Klein4())],
     [Dicyclic(6), direct(Quaternion(2), Cyclic(3)),
      direct(Dicyclic(3), Cyclic(2)), direct(Dihedral(4), Cyclic(3)),
      direct(Symmetric(3), Cyclic(4)), direct(Alternating(4), Cyclic(2)),
      Symmetric(4), Dihedral(12), direct(Dihedral(6), Cyclic(2)),
      ### Q_8\rtimes\Z_3
      ### ???
      ### ???
     ]),

    ([Cyclic(25), elemAbel(5,2)], []),
    ([Cyclic(26)], [Dihedral(13)]),

    ([Cyclic(27), direct(Cyclic(9), Cyclic(3)), elemAbel(3,3)],
     [
      ### ???
      ### ???
     ]),

    ([Cyclic(28), direct(Cyclic(14), Cyclic(2))], [Dihedral(14), Dicyclic(7)]),
    ([Cyclic(29)], []),

    ([Cyclic(30)],
     [Dihedral(15),
      ### ???
      ### ???
     ]),

    ([Cyclic(31)], []),
    ([Cyclic(32), direct(Cyclic(16), Cyclic(2)), direct(Cyclic(8), Cyclic(4)),
      direct(Cyclic(8), Klein4()),
      direct(direct(Cyclic(4), Cyclic(4)), Cyclic(2)),
      direct(Cyclic(4), elemAbel(2,3)), elemAbel(2,5)],
     o32nonA)
]
