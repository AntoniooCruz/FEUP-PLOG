menuDisplay:-
write(' _______________________________________________'),nl,
write('|---------------Welcome to forms!---------------|'),nl,
write('|---------------- 1) P1 vs P2 ------------------|'),nl,
write('|-----------------2) P1 vs COM -----------------|'),nl,
write('|---------------- 3) COM vs P1 -----------------|'),nl,
write('|---------------- 4) COM vs COM ----------------|'),nl,
write('|---------------- 5) EXIT! ---------------------|'),nl,
write('|_______________________________________________|'),nl,nl.

processMenuChoice(Response):-
Response =:= 1,
startPvpGame,nl.

processMenuChoice(Response):-
Response =:= 2,
startCvPGame,nl.

processMenuChoice(Response):-
Response =:= 3,
startPvCGame,nl.

processMenuChoice(Response):-
Response =:= 4,
startCvCGame,nl.

processMenuChoice(Response):-
Response =:= 5,
write('exit '),nl, \+fail.



startPvpGame:-
boardSize(Size),
createBoard(Size,[],Board),
gameCycle(Board,Size,2).

startCvCGame:-
boardSize(Size),
createBoard(Size,[],Board),
readAILevel(Level),
comCycle(Board,Size,2,Level).

startPvCGame:-
boardSize(Size),
createBoard(Size,[],Board),
readAILevel(Level),
gameVComCycle(Board,Size,2,Level).

startCvPGame:-
boardSize(Size),
createBoard(Size,[],Board),
readAILevel(Level),
comVgameCycle(Board,Size,2,Level).