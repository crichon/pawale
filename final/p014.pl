% prolog

% --------------------------
% Partie 1
% --------------------------

no(P):- P, !, fail.
no(_).

element(X, [X|_]).
element(X, [_|Q]):- element(X, Q).


concat([], L, L).
concat([T|Q], L2, [T|L3]):- concat(Q, L2, L3).


mabs(X, AX):- X < 0, AX is 0 - X, ! .
mabs(X, AX):- X >= 0, AX is X, ! .


% appel
genere(MIN, MAX, L):- genere(MIN, MAX, MIN, L).

genere(_, MAX, MAX, [MAX]).
genere(MIN, MAX, Tmp, [Tmp|L]):- Ttmp is Tmp +1, genere(MIN, MAX, Ttmp, L), !.


retire_el(_, [], []).
retire_el(X, [T|Q], [T|LSX]):- X >= T, retire_el(X, Q, LSX), !.
retire_el(X, [_|Q], LSX):- retire_el(X, Q, LSX).

alldiff([]).
alldiff([T|Q]):- element(T, Q), !, fail.
alldiff([_|Q]):- alldiff(Q).

% --------------------------
% Partie 2
% --------------------------

% dans le cas ou L est dans l'ordre croissant
choisirNmarques(0, [], []).
choisirNmarques(N, [T|Q], [T|Regle]):- NN is N - 1, choisirNmarques(NN, Q, Regle).
choisirNmarques(N, [_|Q], Regle):- choisirNmarques(N, Q, Regle).

% L n'est pas forcément dans l'ordre croissant
%cchoisirNmarques(N, L, Regles):- cchoisirNmarques(N, L, [], R).

%cchoisirNmarques(0, [], _, []).
%cchoisirNmarques(N, [T|Q], Skip, Regle):-
    %no(element(T, Skip),
    %NN is N - 1,
    %retire_el(T, choisirNmarques(NN, Q, Skip, Regle).
%cchoisirNmarques(N, [T|Q], Skip, [T|Regle]):-
    %NN is N - 1,
    %retire_el(T, choisirNmarques(NN, Q, Regle).
%cchoisirNmarques(N, [_|Q],Skip, Regle):- choisirNmarques(N, Q, Skip, Regle).

faireIntervalles(_, [], []).
faireIntervalles(M, [T|Q], [TT|I]):- TT is abs(T - M), faireIntervalles(M, Q, I), !.


intervalles(L, R):- intervalles(L, [], R).

intervalles([], L, L).
intervalles([T|Q], L, I):-
    faireIntervalles(T, Q, R),
    concat(L, R, LL),
    intervalles(Q, LL, I), !.


golomb(N, Regle, UB):-
    genere(0, UB, L),
    choisirNnmarques(N, L, [], Regle),
    intervalles(Regle, Check),
    alldiff(Check).


longueur([T], T).
longueur([_|Q], L):-
   longueur(Q, L), !.


meilleurRegle(N, R, L):- meilleurRegle(N, R, L, 1).

meilleurRegle(N, R, _, I):- write('test'), write(I), nl, golomb(N, R, I), ! .
meilleurRegle(N, R, L, I):- write('+1'), nl, II is I + 1, meilleurRegle(N, R, L, II).


consistant(X, R):-
    longueur(R, L), X > L,
    intervalles(R, Check),
    alldiff(Check).

retire_el_inconsistant([], _, []).
retire_el_inconsistant([T|Q], R, LC):- consistant(T, R), retire_el_inconsistant(Q, R, LC), !.
retire_el_inconsistant([T|Q], R, [T|LC]):- retire_el_inconsistant(Q, R, LC).


choisirNnmarques(N, [T|Q], RP, R):-
    %concat(RP, [T], NRP),
    retire_el_inconsistant(Q, RP, LL),
    element(T, LL),
    choisirNnmarques(N, Q, RP, R).

choisirNnmarques(N, [_|Q], RP, R):-
    choisirNnmarques(N, Q, RP, R).

choisirNnmarques(N, [T|Q], RP, R):-
    N > 0,
    concat(RP, [T], NRP),
    NN is N - 1,
    choisirNnmarques(NN, Q, NRP, R) .

choisirNnmarques(0, [], R, R).
