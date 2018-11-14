menuDisplay:-
write(' _______________________________________________'),nl,
write('|---------------Welcome to forms!---------------|'),nl,
write('|---------------- 1) P1 vs P2 ------------------|'),nl,
write('|-----------------2) P1 vs COM -----------------|'),nl,
write('|---------------- 3) COM vs COM ----------------|'),nl,
write('|---------------- 4) EXIT! ---------------------|'),nl,
write('|_______________________________________________|'),nl,nl.


processMenuChoice(Response):-
(Response == '1') -> startPvpGame,nl;
(Response == '2') -> write('P vs C game.'),nl;
(Response == '3') -> startCvCGame,nl;
(Response == '4') -> write('exit '),nl, \+fail.

startPvpGame:-
boardSize(Size),
createBoard(Size,[],Board),
gameCycle(Board,1).

startCvCGame:-
boardSize(Size),
createBoard(Size,[],Board),
comCycle(Board,1).
