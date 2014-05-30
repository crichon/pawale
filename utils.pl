% prolog

diff(X,X):- !, fail.
diff(_,_).

element(X, [T|_]):- X = T, !, true.
element(X, [_|Q]):- element(X, Q).

moves(0, [], L, L).
moves(N, [T|Q], Tmp, R):- diff(T, 0), moves(NI, Q, [NI|Tmp], R), (N is NI +1), !.
moves(N, [_|Q], Tmp, R):- moves(NI, Q, Tmp, R), N is (NI + 1) .
moves(L, R):- moves(_, L, [], R).
