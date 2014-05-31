% prolog

diff(X,X):- !, fail.
diff(_,_).

element(X, [T|_]):- X = T, !, true.
element(X, [_|Q]):- element(X, Q).

concat([], L, L).
concat([X|L1], L2, [X|L3]):- concat(L1, L2, L3).

moves(L, R):- moves(L, 0, R).
moves([T|Q], N, [I|R]) :- diff(T, 0), NN is N +1, moves(Q, NN, R), I is NN, ! .   
moves([_|Q], N, R) :- NN is N +1, moves(Q, NN, R).
moves([], _, []).   

get([_|Q], C, R):- CI is C - 1, get(Q, CI, R), !, true.
get([T|_], 1, T).

add_to_list(N, L, NC, L) :- N = 0, NC is 0, !.
add_to_list(N, [T|Q], NC, [TT|R]):- NI is (N - 1), add_to_list(NI, Q, NC, R), TT is (T + 1), ! .
add_to_list(N, [], N, []).
