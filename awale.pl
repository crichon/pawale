% Prolog file

/* --- Specification ---

Game state is defined by, twoo list
one for each player, ( x, y, ....)
x, y represent the number of seeds by hole, there is six holes by player.
Score is defined by how many seeds a player captured.

in order to simplify the code, we consider

1 2 3 4 5 6
6 5 4 3 2 1

it seems simplier to reverse the list before drawing it and keeping a natural order for processing
*/

% -----------------------------Main Menu ----------------------------- 

launch_game:- repeat, main_menu, !.

main_menu:- write('1. Joueur contre joueur'), nl,
            write('0. Quitter le jeu'), nl,
            nl,
            read(Choice), nl, launch(Choice), nl,
            Choice=0.

launch(0):- write('Au revoir'), ! .
launch(1):- write('Nouvelle partie, humain contre humain'),
            nl, game_loop_pvp([4, 4, 4, 4, 4, 4], [4, 4, 4, 4, 4, 4], 0, 0, 'joueur1'),
            nl, !.

launch(_):- write('Savez-vous lire ?').

% -----------------------------Game Loop ----------------------------- 

diff(X,X):- !, fail.
diff(_,_).

element(X, [T|_]):- X = T, !, true.
element(X, [_|Q]):- element(X, Q).

elements(X, [X|_]).
elements(X, [_|Q]):- element(X, Q).

mooves(_, [], L, L).
mooves(N, [T|Q], Tmp, R):- NI is N +1, diff(T, 0), mooves(NI, Q, [NI|Tmp], R), !.
mooves(N, [_|Q], Tmp, R):- NI is N + 1, mooves(NI, Q, Tmp, R).
mooves(L, R):- mooves(0, L, [], R).

concat(L, LL, R):- concat(L, LL, [], R).
concat(L, LL, Tmp, R):- concat([T|Q], LL, 
concat([], [], U, U).

% How to detect if the game entered in a cycle
% implement other victory condition
is_win(S, Turn):- S >= 25, nl, nl, write(Turn), write(' a gagné. \n Fin de la partie \n\n').

get(L, C, R):- get(1, L, C, R).
get(I, [_|Q], C, R):- II is I +1, get(II, Q, C, R), !. 
get(X, [T|_], X, T).

add_to_list(N, L, NC, R):- add_to_list(N, [], L, NC, R).
add_to_list(N, L, [T|Q], NC, R):- NN is N -1, N > 0, TT is T + 1, add_to_list(NN, [TT|L], Q, NC, R), !.
add_to_list(N, L, [T|Q], NC, R):- add_to_list(N, [T|L], Q, NC, R).
add_to_list(N, T, [], N,T).

%% to test
add_from(N, Pos, L, NC, R):- add_from(0, N, Pos, L, [], NC, R).
add_from(X, N, Pos, [T|Q], L, NC, R):- XI is X + 1, add_from(XI, N, Pos, Q, [T|L], NC, R).
add_from(Pos, N, Pos, [T|Q], T, NC, R):- add_from(1, _, Pos, N, Pos, Q, [0|T], NC, R). 
add_from(1, _, N, _, [T|Q], LL, NC, R):- Ncc is NC - 1, TC is T +1, add_form(1, _, Ncc, _, Q, [TC|LL], NC, R).
add_from(1, _, N, _, [], L, N, L). 

add_froms(N, Pos, L, NC, R):- write('ok'), add_froms(0, N, Pos, L, [], NC, R).
add_froms(Pos, N, Pos, [T|Q], L, NC, R):- add_to_list(N, Q, NC, RR) , write('fuck'), ! . 
add_froms(X, N, Pos, [T|Q], L, NC, R):- XI is (X + 1), add_froms(XI, N, Pos, Q, [T|L], NC, R).
%todo add_skip

% 11 or 12
%seed(I1, I2, Choice, O1, O2):- get(I1, choice, N), X is round(N / 11) + 1, seed(I1, I2, Choice,  
%seed(I1, I2, Choice, O1, O2):- get(I1, Choice, N), add_to_list(N, I1, NN, R), write(NN), nl, write( R).
seed(I1, I2, Choice, O1, O2):- get(I1, Choice, N), seed(I1, I2, N, Choice, [], [], O1, O2).
seed(I1, I2, N, Choice, L1, L2, O1, O2):- get(I1, Choice, Test), N = Test, add_from(N, Choice, I1, NN, L1), add_to_list(NN, Choice, I2, NNN, L2), seed([], [], NN, Choice, L1, L2, O1, O2).
% loop until there is no seed left
seed([], [], N, Choice, O1, O2, L1, L2):-diff(N,0), seed(O1, O2, N Choice, O1, O2, L1, L2). 
seed(I1, I2, N, Choice, L1, L2, O1, O2):- diff(N, 0), add_skip(N, Choice, I2, NN, L2), seed([], I2, NN, Choice, L1, L2, O1, O2), !.
seed([], I2, N, Choice, L1, L2, O1, O2):- diff(N, 0), add_to_list(N, Choice, I2, NN, L2), seed([], [], NN, Choice, L1, L2, O1, O2), !.
seed([], [], 0, Choice, L1, L2, L1, L2).


game_loop_pvp(Map1, Map2, Score1, Score2, Turn):- is_win(Score1, 'joueur1'). 
game_loop_pvp(Map1, Map2, Score1, Score2, Turn):- is_win(Score2, 'joueur2'). 


% ajouter conteur de graine prises
game_loop_pvp(Map1, Map2, Score1, Score2, 'joueur1'):- 
                    draw_game(Map1, Map2, Score1, Score2, 'joueur1'),
                    
                    write('Vous pouvez semer depuis les trous: '),
                    mooves(Map1, R), reverse(R, M), write(M),
                    repeat, nl, read(Choice), element(Choice, M),
                    write(Choice),
                    
                    % redistribute the map and update scores
                    seed(Map1, Map2, Choice, N_map1, N_map2),
                    take(Map2, Score, N_map2, N_score1),
                    game_loop_pvp(N_map1, N_map2, N_score1, Score2, 'joueur2').


game_loop_pvp(Map1, Map2, Score1, Score2, 'joueur2'):- 
                    draw_game(Map1, Map2, Score1, Score2, 'joueur2'),

                    write('Vous pouvez semer depuis les trous: '),
                    mooves(Map2, R), reverse(R, M), write(M),
                    repeat, nl, read(Choice), element(Choice, M),
                    write(Choice),
                    
                    seeding(Map1, Map2, Score2, Choice, N_map1, N_map2, N_score2),
                    game_loop_pvp(N_map1, N_map2, Score1, N_score2, 'joueur2').
% ----------------------------- Draw ----------------------------- 

% reverse a list
reverse([], L, L).
reverse([T|Q], Tmp, Res) :- reverse(Q, [T| Tmp], Res).
reverse(L, LP) :- reverse(L, [], LP).

% draw primitive
draw([T|Q]) :- write(T), tab(4), draw(Q).
draw([]).

% print the map
draw_game(P1, P2, S1, S2, Turn):- 
                    write('\n\n'),
                    write('Joueur 1 |'), tab(2), draw(P1), nl,
                    reverse(P2, RP2),
                    write('Joueur 2 |'), tab(2), draw(RP2), nl,
                    nl, write('Joueur 1:'), tab(2), write(S1), nl,
                    write('Joueur 2:'), tab(2), write(S2), nl,
                    write('Tour de: '), write(Turn), nl,
                    nl, nl, nl.

