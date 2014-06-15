% main.pl
% convention map 1 is the playing player, map2 is for the opponement
% same goes on for score1 and score2

% ------------------------------------------------------------------------------
% UI
% ------------------------------------------------------------------------------


% draw primitive
draw([T|Q]) :- write(T), tab(4), draw(Q).
draw([]).

% print the map
draw_game(P1, P2, S1, S2, Turn):-
                    write('\n\n'),
                    reverse(P2, RP2),
                    write('Joueur 2 |'), tab(2), draw(RP2), nl,
                    write('Joueur 1 |'), tab(2), draw(P1), nl,
                    nl, write('Joueur 1:'), tab(2), write(S1), nl,
                    write('Joueur 2:'), tab(2), write(S2), nl,
                    write('Tour de: '), write(Turn), nl,
                    nl, nl, nl.

% print the map
debug(P1, P2):-
                    write('\n\n'),
                    write('-----------------------------DEBUG-------------------------------------------'),
                    nl,
                    reverse(P2, RP2),
                    write('Joueur 2 |'), tab(2), draw(RP2), nl,
                    write('Joueur 1 |'), tab(2), draw(P1), nl,
                    write('------------------------------------------------------------------------------'),
                    nl, nl, nl.


% ------------------------------------------------------------------------------
% Primitive
% ------------------------------------------------------------------------------


no(P):- P, ! , fail.
no(_).

diff(X,X):- !, fail.
diff(_,_).

element(X, [T|_]):- X = T, true.
element(X, [_|Q]):- element(X, Q).

concat([], L, L).
concat([X|L1], L2, [X|L3]):- concat(L1, L2, L3).

split([T|Q], NN, [T|L1], L2):- N is NN - 1, split(Q, N, L1, L2), !.
split(L, 0, [], L).

% reverse a list
reverse([], L, L).
reverse([T|Q], Tmp, Res) :- reverse(Q, [T| Tmp], Res).
reverse(L, LP) :- reverse(L, [], LP).

sum([], 0).
sum([T|Q], S):- sum(Q, Res), S is Res + T.


% ------------------------------------------------------------------------------
% Utils
% ------------------------------------------------------------------------------



moves(L, R):- moves(L, 0, R).
moves([T|Q], N, [I|R]) :- diff(T, 0), NN is N +1, moves(Q, NN, R), I is NN, ! .
moves([_|Q], N, R) :- NN is N +1, moves(Q, NN, R).
moves([], _, []).

% take maps, if all opponement fields are null, check in the allowed moves (L) if we can feed him
check_moves_f_null(_, Map2, L, R):- diff(Map2, [O,O,O,O,O,O]), !, concat([], L, R).

check_moves_f_null(Map1, Map2, [T|Q], [T|R]):-
    no(launch_seed(Map1, Map2, T, _, [0, 0, 0, 0, 0, 0], _)),
    check_moves_f_null(Map1, Map2, Q, R), ! .

check_moves_f_null(Map1, Map2, [_|Q], R):- check_moves_f_null(Map1, Map2, Q, R), !.

check_moves_f_null(_, _, [], []).


check_moves(Map, Choice) :- repeat, check(Map, Choice), !.

check(Map, Choice):- nl, read(Choice), element(Choice, Map).
                    %repeat, nl, read(Choice), element(Choice, R),

% ------------------------------------------------------------------------------
% Seed -> Distribute the seeds, update the maps and return pf, final position
% ------------------------------------------------------------------------------

launch_seed(Map1, Map2, Choice, Nmap1, Nmap2, Pf):-
                    concat(Map1, Map2, Map),
                    seed(Map, Choice, Nmap, Pf),
                    split(Nmap, 6, Nmap1, Nmap2).


% lack elegance, but well it's working
seed(Map, C, R, Pf):- seed(1, Map, [], C, R, Pf).
seed(I, [T|Q], L, C, Rf, Pf):- diff(I, C), II is I +1, seed(II, Q, [T|L], C, Rf, Pf), !.
seed(I, [T|Q], L, C, Rf, Pf):- I = C, II is I +1, seed(II, Q, [0|L], C, T, Rf, Pf), ! .
% loop
seed(I, [T|Q], R, C, N, Rf, Pf):- diff(I, C), diff(N, 0), II is I + 1, NN is N - 1, TT is T +1, seed(II, Q, [TT|R], C, NN, Rf, Pf), !.
seed(I, [_|Q], R, C, N, Rf, Pf):- I = C, diff(N, 0), II is I + 1, seed(II, Q, [0|R], C, N, Rf, Pf), !.
seed(_, [], R, C, N, Rf, Pf):- diff(N, 0), reverse(R, RR),nl, seed(1, RR, [], C, N, Rf, Pf), ! .
%end
seed(I, L, R, _, 0, X, II):- II is I - 1, reverse(R,RR), concat(RR, L, X), ! .


% ------------------------------------------------------------------------------
% Take (Prise)
% ------------------------------------------------------------------------------


update([T|Q], Score, R, [0|M]):- T =< 3, T >= 2, S is Score + T, update(Q, S, R, M), ! .
update(L, R, R, L).

take(Pf, Map, Score, NS, N_map):- Pf = 0, take(12, Map, Score, NS, N_map), !.
take(Pf, Map, Score, Ns, N_map):- Pf =< 6, Ns is Score, concat(Map, [], N_map), !.
take(Pf, Map, Score, NS, N_map):-
                    %write('wtf'), nl,
                    N is Pf - 6, split(Map, N, Take, Res),
                    reverse(Take, RTake),
                    update(RTake, Score, NS, NRTake),
                    reverse(NRTake, N_tmap2), concat(N_tmap2, Res, N_map),
                    % check if the new map is not null
                    diff(N_map, [0,0,0,0,0,0]), !.

take(Pf, Map, Score, NS, N_map):-
                    N is Pf - 6, split(Map, N, Take, Res),
                    reverse(Take, RTake),
                    update(RTake, Score, _, NRTake),
                    reverse(NRTake, N_tmap2), concat(N_tmap2, Res, N_map),
                    NS is Score ,
                    write('Champs adverses vidés, vous ne gagnez pas de points'), nl.


% ------------------------------------------------------------------------------
% Main menu
% ------------------------------------------------------------------------------


launch_game:- repeat, main_menu, !.

main_menu:- write('1. Joueur contre joueur'), nl,
            write('0. Quitter le jeu'), nl,
            nl,
            read(Choice), nl, launch(Choice), nl,
            Choice=0.

launch(0):- write('Au revoir'), ! .
launch(1):- write('Nouvelle partie, humain contre humain'),
            nl, game_loop_pvp([4, 4, 4, 4, 4, 4], [4, 4, 4, 4, 4, 4], 0, 0, 'joueur1', []),
            nl, !.

launch(_):- write('Savez-vous lire ?').

% ------------------------------------------------------------------------------
% Game loop
% ------------------------------------------------------------------------------
is_win(S, Turn):- S >= 25, nl, nl, write(Turn), write(' a gagné. \n Fin de la partie \n\n').

% to factorize
% current player can't play, the game ends, player 2 get all his seeds
is_finnish([0,0,0,0,0,0], Map2, Score1, Score2, Turn):- write('Vous ne pouvez plus jouer, votre adversaire récupere ses graines.'),
    sum(Map2, S), NS is Score2 + S, winner(Score1, NS, Turn). 

winner(S1, S2, Turn):- S1 > S2, write(Turn), write(' a gagné, '), write(S1), write(' à '), write(S2), !. 
winner(S1, S2, _):- S1 = S2, write(' égalité, '), write(S1), write(' à '), write(S2). 
winner(S1, S2, Turn):- S1 < S2, write(Turn), write(' a perdu, '), write(S1), write(' à '), write(S2), !.

is_cycle(T, Q, Score1, Score2, Turn):- element(T, Q), %game end
    write('Le jeu boucle, fin de la partie'), winner(Score1, Score2, Turn).

% factorize using turn ?
game_loop_pvp(Map1, Map2, Score1, Score2, Turn, [T|Q]):-
    Turn = 'joueur1',
    is_win(Score2, 'joueur2'), % plyer 2 just played, check if he has win
    is_finnish(Map1, Map2, Score1, Score2, Turn), % check if i can play
    % determine if the game is cycling, each node is composed of the (map(1->12), pf) 
    is_cycle(T, Q, Score1, Score2, Turn).

game_loop_pvp(Map1, Map2, Score1, Score2, Turn, [T|Q]):-
    Turn = 'joueur2',
    is_win(Score1, 'joueur1'),
    is_end(Map2, Map1, Score2, Score1, Turn),
    is_cycle(T, Q, Score2, Score1, Turn). 

% factoriser en inversant les listes ?
game_loop_pvp(Map1, Map2, Score1, Score2, 'joueur1', G):-
                    draw_game(Map1, Map2, Score1, Score2, 'joueur1'),

                    write('Vous pouvez semer depuis les trous: '),
                    moves(Map1, R), check_moves_f_null(Map1, Map2, R, M),
                    write(M), nl,
                    check_moves(M, Choice),
                    write(Choice), nl,

                    % redistribute the map and update scores
                    % to factorize
                    %debug(Map1, Map2),
                    launch_seed(Map1, Map2, Choice, N_map1, N_map2, Pf),
                    write('before'),

                    debug(N_map1, N_map2),
                    % take
                    take(Pf, N_map2, Score1, NS, U_map2),
                    concat(N_map1, U_map2, GMap),
                    concat(G, [(GMap, Pf)], GG),
                    game_loop_pvp(N_map1, U_map2, NS, Score2, 'joueur2', GG).


game_loop_pvp(Map1, Map2, Score1, Score2, 'joueur2', G):-
                    draw_game(Map1, Map2, Score1, Score2, 'joueur2'),

                    write('Vous pouvez semer depuis les trous: '),
                    moves(Map2, R), check_moves_f_null(Map2, Map1, R, M),
                    write(M), nl,
                    check_moves(M, Choice),
                    write(Choice), nl,

                    % redistribute the map and update scores
                    % to factorize
                    launch_seed(Map2, Map1, Choice, N_map2, N_map1, Pf),
                    %concat(Map2, Map1, Map),
                    %seed(Map, Choice, Nmap, Pf),
                    %split(Nmap, 6, N_map2, N_map1),

                    debug(N_map1, N_map2),
                    % take
                    take(Pf, N_map1, Score2, NS, U_map1),

                    concat(U_map1, N_map2, GMap),
                    concat(G, [(GMap, Pf)], GG),

                    game_loop_pvp(U_map1, N_map2, Score1, NS, 'joueur1', GG).
