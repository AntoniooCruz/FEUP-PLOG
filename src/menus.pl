% Displays the Main menu 

menuDisplay:-
write(' _______________________________________________'),nl,
write('|---------------Welcome to forms!---------------|'),nl,
write('|---------------- 1) P1 vs P2 ------------------|'),nl,
write('|-----------------2) P1 vs COM -----------------|'),nl,
write('|---------------- 3) COM vs P1 -----------------|'),nl,
write('|---------------- 4) COM vs COM ----------------|'),nl,
write('|---------------- 5) EXIT! ---------------------|'),nl,
write('|_______________________________________________|'),nl,nl.


% Processes the response of the user to the Main menu 
% processMenuChoice(+Response)

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


% Initiates the Player vs Player game 

startPvpGame:-
boardSize(Size),
createBoard(Size,[],Board),
gameCycle(Board,Size,2).

% Initiates the Com vs Com game 
startCvCGame:-
boardSize(Size),
createBoard(Size,[],Board),
readAILevel(Level),
comCycle(Board,Size,2,Level).

% Initiates the Player vs Com game 

startPvCGame:-
boardSize(Size),
createBoard(Size,[],Board),
readAILevel(Level),
gameVComCycle(Board,Size,2,Level).

% Initiates the Com vs Player game 

startCvPGame:-
boardSize(Size),
createBoard(Size,[],Board),
readAILevel(Level),
comVgameCycle(Board,Size,2,Level).