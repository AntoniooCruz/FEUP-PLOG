:- use_module(library(lists)).
:- use_module(library(random)).
/*Aux function for createEmptyBoard*/
createEmptyRow(0, []).
createEmptyRow(X,[0 | Row]):-
X1 is X - 1,
createEmptyRow(X1,Row).

/*Aux function for createEmptyBoard*/
createEmptyColumns(0, Row, []).
createEmptyColumns(Y, Row, [Row | Board]):-
Y1 is Y - 1,
createEmptyColumns(Y1, Row, Board).

/*Creates an empty board with dimensions X*Y */
createEmptyBoard(Board, X, Y):-
createEmptyRow(X,Row),
createEmptyColumns(Y, Row,Board).


/*Inserts an element(1) in a list(0) on index(2), returns a list(4)*/
insert([], Element , 0, [Element | NewList]):- insert([], Element, -1, NewList).
insert([],_,_,[]). 
insert([H|T], Element, 0, [Element | NewList]):- insert([H|T], Element, -3, NewList).
insert([H|T], Element, Index, [H | NewList]):-
Index1 is Index -1,
insert(T, Element, Index1, NewList).

/*Creates a dummy board*/
createBoard(X):-
append([[1,0,0],[0,1,0]], [[0,0,1]], X).

/*Gets the character in Board[Y][X]*/
getPos(X,Y,Element,Board):-
nth0(Y, Board, Row),
nth0(X, Row, Element).

/*Makes a players play, replaces the cell in coordinates (0,1) with the character(2), on a board(3), returns the resulting board(4)*/
insertPiece(X, Y, Player, OldB, NewB):-
nth0(Y, OldB, Row, RestRows),
nth0(X, Row, Element, RestOfRow),
insert(RestOfRow, Player, X, NewRow),
insert(RestRows, NewRow, Y, NewB).

/*Checks if a play is valid, the player can only move his stones to a place with an opponent's stone*/
validPlay(X,Y,Player,Board):-
getPos(X,Y,Element,Board),
Element \= Player, Element \= 0.

/*Checks for valid play, removes a players stone from one cell and places it in another cell, removes the opponent's stone*/
makePlay([X1,Y1,X2,Y2],Player,OldB,NewB):-
insertPiece(X1,Y1,0,OldB,OldB1),
insertPiece(X2,Y2,Player,OldB1,NewB).

checkUp(X,Y,OldB, NewB):-
Y1 is Y - 1, 
nth0(Y1, OldB, Row),
nth0(X, Row, Element),
(Element == 1 ; Element == 2) -> removePieces(X,Y1,Element,OldB,NewB); removePieces(X,Y,Element,[],NewB).


checkDown(X,Y,OldB, NewB):-
Y1 is Y + 1, 
nth0(Y1, OldB, Row),
nth0(X, Row, Element),
(Element == 1 ; Element == 2) -> removePieces(X,Y1, Element, OldB, NewB); removePieces(X,Y,Element,[],NewB).



checkLeft(X,Y,OldB,NewB):-
X1 is X - 1, 
nth0(Y, OldB, Row),
nth0(X1, Row, Element),
(Element == 1 ; Element == 2) -> removePieces(X1,Y, Element, OldB, NewB); removePieces(X,Y,Element,[],NewB).



checkRight(X,Y,OldB,NewB):-
X1 is X + 1, 
nth0(Y, OldB, Row),
nth0(X1, Row, Element),
(Element == 1 ; Element == 2) -> removePieces(X1,Y, Element, OldB, NewB); removePieces(X,Y,Element,[],NewB).

checkCoordinates(X,Y,[]):-fail.
checkCoordinates(X,Y,[H|T]):-
compareCoords(X,Y,H);
checkCoordinates(X,Y,T).

compareCoords(X,Y,[H|T]):-
X == H,
compareCoords2(Y,T).

compareCoords2(Y,[H|T]):- Y == H.


/*[[0,0,0,0,0,0,0],[0,0,0,1,2,1,0],[2,0,0,1,0,1,0],[1,2,2,1,0,2,1],[0,1,0,2,2,1,2],[2,0,1,0,0,0,2],[1,2,2,0,1,1,2]]*/
removePieces(X,Y,Element,[],New).
/*removePieces(X,Y,OldB, 1):- removePieces(X, Y, OldB, [[0,0,0,0,0,0],[0,0,0,0,0,0],[0,0,0,0,0,0],[0,0,0,0,0,0],[0,0,0,0,0,0],[0,0,0,0,0,0]]).*/
/*A substituição de células por 5 não está a funcionar talvez seja melhor usar a Lista para verificar quais as células visitadas*/
removePieces(X, Y, Element, OldB, [[X,Y,Element] | NewB]):-
insertPiece(X, Y, 5, OldB, OldB1),
(Y \= 0 -> checkUp(X,Y,OldB1,New1); 1=1),
(Y \= 6 -> checkDown(X,Y,OldB1,New2); 1=1),
(X \= 0 -> checkLeft(X,Y,OldB1,New3); 1=1),
(X \= 6 -> checkRight(X,Y,OldB1,New4); 1=1),
append(New1,New2,NewA1),
append(NewA1,New3,NewA2),
append(NewA2, New4, NewB).

removePieces2(X, Y, OldB, NewB):-
insertPiece(X, Y, 5, OldB, OldB1),
(Y \= 0 -> checkUp(X,Y,OldB1,New1); 1=1),
(Y \= 6 -> checkDown(X,Y,OldB1,New2); 1=1),
(X \= 0 -> checkLeft(X,Y,OldB1,New3); 1=1),
(X \= 6 -> checkRight(X,Y,OldB1,New4); 1=1),
append(New1,New2,NewA1),
append(NewA1,New3,NewA2),
append(NewA2, New4, NewB).


/*Verifies if a certain move is valid*/
getPos(X,Y,Element,Board):-
nth0(Y, Board, Row),
nth0(X, Row, Element).

validMove([Xs,Ys,Xf,Yf],Board,Player):-
getPos(Xs,Ys,ElementStart,Board),
getPos(Xf,Yf,ElementEnd,Board),
ElementStart =:= Player,
ElementEnd =\= Player,
ElementEnd =\= 0.

/*Chooses a Random Play*/
getRandomPlay(Board,Player,[Xs,Ys,Xf,Yf]):-
findall([Xs,Ys,Xf,Yf],validMove([Xs,Ys,Xf,Yf],Board,Player),ListOfMoves),
list_length(ListOfMoves,Size),
random(0,Size,Move),
nth0(Move,ListOfMoves,[Xs,Ys,Xf,Yf]).


/*Calculates the length of a list*/
list_length(Xs,L) :- list_length(Xs,0,L) .

list_length( []     , L , L ) .
list_length( [_|Xs] , T , L ) :-
  T1 is T+1 ,
  list_length(Xs,T1,L).



