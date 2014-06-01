% main.pl

% ------------------------------------------------------------------------------
% Utils
% ------------------------------------------------------------------------------

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

% lack elegance, but well it's working
seed(Map, C, R, Pf):- seed(1, Map, [], C, R, Pf).
seed(I, [T|Q], L, C, Rf, Pf):- diff(I, C), II is I +1, seed(II, Q, [T|L], C, Rf, Pf), !.
seed(I, [T|Q], L, C, Rf, Pf):- I = C, II is I +1, seed(II, Q, [0|L], C, T, Rf, Pf), ! .
% loop
seed(I, [T|Q], R, C, N, Rf, Pf):- diff(I, C), diff(N, 0), II is I + 1, NN is N - 1, TT is T +1, seed(II, Q, [TT|R], C, NN, Rf, Pf), !.
seed(I, [_|Q], R, C, N, Rf, Pf):- I = C, diff(N, 0), II is I + 1, seed(II, Q, [0|R], C, N, Rf, Pf), !.
seed(_, [], R, C, N, Rf, Pf):- reverse(R, RR),nl, seed(1, RR, [], C, N, Rf, Pf), ! .
%end
seed(I, L, R, _, 0, X, II):- II is I - 1, reverse(R,RR), concat(RR, L, X), ! .

split([T|Q], NN, [T|L1], L2):- N is NN - 1, split(Q, N, L1, L2), !.
split(L, 0, [], L).


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
            nl, game_loop_pvp([4, 4, 4, 4, 4, 4], [4, 4, 4, 4, 4, 4], 0, 0, 'joueur1'),
            nl, !.

launch(_):- write('Savez-vous lire ?').


% ------------------------------------------------------------------------------
% Game loop 
% ------------------------------------------------------------------------------
is_win(S, Turn):- S >= 25, nl, nl, write(Turn), write(' a gagné. \n Fin de la partie \n\n').

% factorize using turn ?
game_loop_pvp(Map1, Map2, Score1, Score2, Turn):- is_win(Score1, 'joueur1'). 
game_loop_pvp(Map1, Map2, Score1, Score2, Turn):- is_win(Score2, 'joueur2'). 

% ajouter conteur de graine prises
game_loop_pvp(Map1, Map2, Score1, Score2, 'joueur1'):- 
                    draw_game(Map1, Map2, Score1, Score2, 'joueur1'),
                    
                    write('Vous pouvez semer depuis les trous: '),
                    mooves(Map1, R), write(R),
                    repeat, nl, read(Choice), element(Choice, R),
                    write(Choice),
                    
                    % redistribute the map and update scores
                    reverse(Map2, RMap2),
                    concat(Map1, RMap2, Map).
                    seed(Map, Choice, Nmap, Pf),
                    split(Nmap, 6, N_map1, RN_map2),
                    reverse(RN_map2, N_map2),

                    take(Map2, Score, N_map2, N_score1),
                    game_loop_pvp(N_map1, N_map2, N_score1, Score2, 'joueur2').


game_loop_pvp(Map1, Map2, Score1, Score2, 'joueur2'):- 
                    draw_game(Map1, Map2, Score1, Score2, 'joueur2'),

                    write('Vous pouvez semer depuis les trous: '),
                    mooves(Map2, R), write(R),
                    repeat, nl, read(Choice), element(Choice, R),
                    write(Choice),
                    
                    seed(Map, Choice, Nmap, Pf),
                    split(Nmap, 6, N_map1, N_map2),
                    game_loop_pvp(N_map1, N_map2, Score1, N_score2, 'joueur2').
