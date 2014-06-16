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
                    write('-----------------------------Nouveau tour-------------------------------------------'),
                    nl, reverse(P2, RP2),
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
% Victory conditions
% ------------------------------------------------------------------------------

is_win(S, Turn):- S >= 25, nl, nl, write(Turn), write(' a gagné. \n Fin de la partie \n\n'), !.

% current player can't play, the game ends, player 2 get all his seeds
is_finnish([0,0,0,0,0,0], Map2, Score1, Score2, Turn):- write('Vous ne pouvez plus jouer, votre adversaire récupere ses graines.'),
    sum(Map2, S), NS is Score2 + S, winner(Score1, NS, Turn).

winner(S1, S2, Turn):- S1 > S2, write(Turn), write(' a gagné, '), write(S1), write(' à '), write(S2), !.
winner(S1, S2, _):- S1 = S2, write(' égalité, '), write(S1), write(' à '), write(S2).
winner(S1, S2, Turn):- S1 < S2, write(Turn), write(' a perdu, '), write(S1), write(' à '), write(S2), !.

is_cycle(T, Q, Score1, Score2, Turn):- element(T, Q), %game end
    write('Le jeu boucle, fin de la partie'), winner(Score1, Score2, Turn), !.


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

play(Map1, Map2, Score1, NS, N_map1, U_map2, Pf, Choice):-
                    moves(Map1, R), check_moves_f_null(Map1, Map2, R, M),
                    element(Choice, M),
                    launch_seed(Map1, Map2, Choice, N_map1, N_map2, Pf),
                    take(Pf, N_map2, Score1, NS, U_map2).


best_play([[Score, _, M2, _, Choice]| _], Score2, Play, _, ES, _, _):-
        M2 = [0, 0, 0, 0, 0, 0], Score > Score2, ES is Score,
        write('Vous pouvez gagné en jouant'), write(Choice), nl,
        Play is Choice, ! .

best_play([[Score, _, _, _, Choice]| _], _, Play, _, ES, _, _):-
    Score > 25, ES is Score,
    write('Vous pouvez gagné en jouant'), write(Choice), nl,
    Play is Choice, ! .

best_play([[Score, _, _, _, Choice]| Q], Score2, Play, Tmp, ES, CL, Score1):-
    %write(Play),
    Score > Tmp, Ttmp is Score, Play is Choice,
    write(Score), write(' '), write(Choice), nl, write(' Play is: '), write(Play), write('/'), write(Ttmp), nl,
    %write('coup'), write(Play), nl,
    best_play(Q, Score2, Play, Ttmp, ES, [Play|CL], Score1).

best_play([[Score, _, _, _, Choice]| Q], Score2, Play, Tmp, ES, CL, Score1):-
    write(Score), write(' '), write(Choice), write(' Play is: '), write(Play),nl,
    best_play(Q, Score2, Play, Tmp, ES, [Choice|CL], Score1)
    .

best_play([], Score2, Play, Tmp, ES, CL, Score1):-
    write('wtf'), write(Tmp), write(Score2),
    Tmp = Score1,
    ES = 'pas de changement sur le socre', test_rand(CL, Rand), Play is Rand.

best_play([], _, _, Tmp, ES, _, _):- 
    ES is Tmp. 

test_rand(CL, Rand):- repeat, write('fuck'), is_ok(CL, Rand). 

is_ok(CL, Rand):- Play is random(6) + 1, write(Play), element(Play, CL), Rand is Play.


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
            write('2. Joueur contre ordinateur'), nl,
            write('3. Ordinateur contre ordinateur'), nl,
            write('0. Quitter le jeu'), nl,
            nl,
            read(Choice), nl, launch(Choice), nl,
            Choice=0.

launch(0):- write('Au revoir'), ! .
launch(1):- write('Nouvelle partie, humain contre humain'),
            nl, game_loop_pvp([4, 4, 4, 4, 4, 4], [4, 4, 4, 4, 4, 4], 0, 0, 'joueur1', []),
            nl, !.

launch(2):- write('Nouvelle partie, humain contre ordinateur'),
            nl, game_loop_pvc([4, 4, 4, 4, 4, 4], [4, 4, 4, 4, 4, 4], 0, 0, 'joueur2', []),
            nl, !.

launch(3):- write('Nouvelle partie, humain contre ordinateur'),
            nl, game_loop_cvc([4, 4, 4, 4, 4, 4], [4, 4, 4, 4, 4, 4], 0, 0, 'joueur2', []),
            nl, !.

launch(_):- write('Savez-vous lire ?').

% ------------------------------------------------------------------------------
% Game loop
% game_loop pvc, pvp, cvc
% to factorize
% ------------------------------------------------------------------------------

%------------------pvp----------------


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
    is_finnish(Map2, Map1, Score2, Score1, Turn),
    is_cycle(T, Q, Score2, Score1, Turn).


% factoriser en inversant les listes ?
game_loop_pvp(Map1, Map2, Score1, Score2, 'joueur1', G):-
                    draw_game(Map1, Map2, Score1, Score2, 'joueur1'),

                    write('Vous pouvez semer depuis les trous: '),
                    moves(Map1, R), check_moves_f_null(Map1, Map2, R, M),
                    write(M), nl,
                    bagof([NS, N_map1, U_map2, Pf, Choice], play(Map1, Map2, Score1, NS, N_map1, U_map2, Pf, Choice), Z),
                    %nl, best_play(Z, Score2, Play, 0, ES, []),
                    nl, best_play(Z, Score2, Play, Score1, ES, [], Score1),
                    write('Maximum de gains par le coup:'), write(Play), write(' Score: '), write(ES), nl,
                    check_moves(M, Choice),
                    write(Choice), nl,

                    launch_seed(Map1, Map2, Choice, N_map1, N_map2, Pf),

                    %debug(N_map1, N_map2),
                    take(Pf, N_map2, Score1, NS, U_map2),
                    %debug(Map1, Map2),

                    concat(N_map1, U_map2, GMap),
                    concat(G, [(GMap, Pf)], GG),
                    game_loop_pvp(N_map1, U_map2, NS, Score2, 'joueur2', GG).


game_loop_pvp(Map1, Map2, Score1, Score2, 'joueur2', G):-
                    draw_game(Map1, Map2, Score1, Score2, 'joueur2'),

                    write('Vous pouvez semer depuis les trous: '),
                    moves(Map2, R), check_moves_f_null(Map2, Map1, R, M),
                    write(M), nl,
                    bagof([NS, U_map1, N_map2, Pf, Choice], play(Map2, Map1, Score2, NS, U_map1, N_map2, Pf, Choice), Z),
                    nl, best_play(Z, Score1, Play, Score2, ES, [], Score2),
                    write('Maximum de gains par le coup:'), write(Play), write(' Score: '), write(ES), nl,
                    check_moves(M, Choice),
                    write(Choice), nl,

                    launch_seed(Map2, Map1, Choice, N_map2, N_map1, Pf),

                    take(Pf, N_map1, Score2, NS, U_map1),

                    concat(U_map1, N_map2, GMap),
                    concat(G, [(GMap, Pf)], GG),

                    game_loop_pvp(U_map1, N_map2, Score1, NS, 'joueur1', GG).

%------------------pvc----------------

game_loop_pvc(Map1, Map2, Score1, Score2, Turn, [T|Q]):-
    Turn = 'joueur1',
    is_win(Score2, 'joueur2'), % plyer 2 just played, check if he has win
    is_finnish(Map1, Map2, Score1, Score2, Turn), % check if i can play
    % determine if the game is cycling, each node is composed of the (map(1->12), pf)
    is_cycle(T, Q, Score1, Score2, Turn).


game_loop_pvc(Map1, Map2, Score1, Score2, Turn, [T|Q]):-
    Turn = 'joueur2',
    is_win(Score1, 'joueur1'),
    is_finnish(Map2, Map1, Score2, Score1, Turn),
    is_cycle(T, Q, Score2, Score1, Turn).


% factoriser en inversant les listes ?
game_loop_pvc(Map1, Map2, Score1, Score2, 'joueur1', G):-
                    draw_game(Map1, Map2, Score1, Score2, 'joueur1'),

                    write('Vous pouvez semer depuis les trous: '),
                    moves(Map1, R), check_moves_f_null(Map1, Map2, R, M),
                    write(M), nl,
                    bagof([NS, N_map1, U_map2, Pf, Choice], play(Map1, Map2, Score2, NS, N_map1, U_map2, Pf, Choice), Z),
                    %nl, best_play(Z, Score2, Play, 0, ES, []),
                    nl, best_play(Z, Score2, Play, Score1, ES, [], Score1),
                    write('Maximum de gains par le coup:'), write(Play), write(' Score: '), write(ES), nl,
                    check_moves(M, Choice),
                    write(Choice), nl,
                    write(Play), nl,

                    launch_seed(Map1, Map2, Play, N_map1, N_map2, Pf),
                    write(Pf), write(N_map2), write(Score1), write(NS), write(U_map2),

                    take(Pf, N_map2, Score1, NS, U_map2),

                    concat(N_map1, U_map2, GMap),
                    concat(G, [(GMap, Pf)], GG),
                    game_loop_pvc(N_map1, U_map2, NS, Score2, 'joueur2', GG).


game_loop_pvc(Map1, Map2, Score1, Score2, 'joueur2', G):-
                    draw_game(Map1, Map2, Score1, Score2, 'joueur2'),

                    bagof([NS, U_map1, N_map2, Pf, Choice], play(Map2, Map1, Score2, NS, U_map1, N_map2, Pf, Choice), Z),
                    nl, best_play(Z, Score1, Play, Score2, _, [], Score2), nl,
                    write('coup: '), write(Play),

                    launch_seed(Map2, Map1, Play, N_map2, N_map1, Pf),

                    take(Pf, N_map1, Score2, NS, U_map1),

                    concat(U_map1, N_map2, GMap),
                    concat(G, [(GMap, Pf)], GG),

                    game_loop_pvc(U_map1, N_map2, Score1, NS, 'joueur1', GG).


%------------------pvc----------------


game_loop_cvc(Map1, Map2, Score1, Score2, Turn, [T|Q]):-
    Turn = 'joueur1',
    is_win(Score2, 'joueur2'), % plyer 2 just played, check if he has win
    is_finnish(Map1, Map2, Score1, Score2, Turn), % check if i can play
    % determine if the game is cycling, each node is composed of the (map(1->12), pf)
    is_cycle(T, Q, Score1, Score2, Turn).


game_loop_cvc(Map1, Map2, Score1, Score2, Turn, [T|Q]):-
    Turn = 'joueur2',
    is_win(Score1, 'joueur1'),
    is_finnish(Map2, Map1, Score2, Score1, Turn),
    is_cycle(T, Q, Score2, Score1, Turn).


% factoriser en inversant les listes ?
game_loop_cvc(Map1, Map2, Score1, Score2, 'joueur1', G):-
                    draw_game(Map1, Map2, Score1, Score2, 'joueur1'),
                    sleep(2),
                    bagof([NS, N_map1, U_map2, Pf, Choice], play(Map1, Map2, Score2, NS, N_map1, U_map2, Pf, Choice), Z),
                    nl, best_play(Z, Score2, Play, Score1, _, [], Score1),nl,
                    write('coup: '), write(Play),

                    launch_seed(Map1, Map2, Play, N_map1, N_map2, Pf),

                    take(Pf, N_map2, Score1, NS, U_map2),

                    concat(N_map1, U_map2, GMap),
                    concat(G, [(GMap, Pf)], GG),
                    game_loop_cvc(N_map1, U_map2, NS, Score2, 'joueur2', GG).


game_loop_cvc(Map1, Map2, Score1, Score2, 'joueur2', G):-
                    draw_game(Map1, Map2, Score1, Score2, 'joueur2'),

                    sleep(2),
                    bagof([NS, U_map1, N_map2, Pf, Choice], play(Map2, Map1, Score2, NS, U_map1, N_map2, Pf, Choice), Z),
                    nl, best_play(Z, Score1, Play, Score2, _, [], Score2), nl,
                    write('coup: '), write(Play),

                    launch_seed(Map2, Map1, Play, N_map2, N_map1, Pf),

                    take(Pf, N_map1, Score2, NS, U_map1),

                    concat(U_map1, N_map2, GMap),
                    concat(G, [(GMap, Pf)], GG),

                    game_loop_cvc(U_map1, N_map2, Score1, NS, 'joueur1', GG).
