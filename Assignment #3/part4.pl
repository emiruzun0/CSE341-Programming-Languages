element(E, [E|_]).							%Check the element E is in list L 
element(E, [_|L]):- element(E, L).
union(L1,L2,L3) :- union2(L1,L2,X), equivalent(X,L3).		%It checks the L3 is equal to union of list L1 and L2
union2([], L2, L2).											
union2([E|L1], L2, L3) :- element(E, L2), !, union2(L1, L2, L3). 
union2([E|L1], L2, [E|L3]) :- union2(L1, L2, L3).
equivalent(L1, L2) :-permutation(L1, L2) .						%It checks list L1 and L2 is equal to each other.
intersect(L1,L2,L3) :- intersect2(L1,L2,X), equivalent(X,L3).		%It checks the L3 is equal to intersect of list L1 and L2
intersect2([], _, []).
intersect2([E|L1], L2, [E|L3]):- element(E, L2), !, intersect2(L1, L2, L3).
intersect2([_|L1], L2, L3):- intersect2(L1, L2, L3).