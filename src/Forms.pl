:-ensure_loaded(input).
:-ensure_loaded(menus).
:-ensure_loaded(display).
:-ensure_loaded(logic).

forms:-
menuDisplay,
getChar(Opt),
processMenuChoice(Opt).



processMenuChoice(Response):-
(Response == '1') -> startPvpGame,nl;
(Response == '2') -> write('P vs C game.'),nl;
(Response == '3') -> write('C vs C game.'),nl;
(Response == '4') -> write('exit '),nl, \+fail.

startPvpGame:-
createBoard(6,[],Board),
gameCycle(Board,1).

gameCycle(Board,Player):-
print_tab(Board,0),nl,
write('Player '),write(Player),nl,
write('Piece that you want to move: '),nl,
write('Collumn: '),
getChar(XsChar),
number_chars(Xs,[XsChar]),nl,
write('Row: '),
getChar(YsChar),
number_chars(Ys,[YsChar]),nl,
write('Place to move the piece: '),nl,
write('Collumn: '),
getChar(XfChar),
number_chars(Xf,[XfChar]),nl,
write('Row: '),
getChar(YfChar),
number_chars(Yf,[YfChar]),nl,
makePlay(Xs,Ys,Xf,Yf,Player,Board,NewBoard),
(
    Player =:= 1 -> gameCycle(NewBoard,2);
    gameCycle(NewBoard,1)
).





