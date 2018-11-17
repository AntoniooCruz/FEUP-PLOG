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
makePlay(X1,Y1,X2,Y2,Player,OldB,NewB):-
validPlay(X2,Y2,Player,OldB) -> (insertPiece(X1,Y1,0,OldB,OldB1),
insertPiece(X2,Y2,Player,OldB1,NewB)); (write('Invalid play!'), fail).





/*Checks if an element in the list has the coordinates given*/
checkCoordinates(X,Y,[]):-fail.
checkCoordinates(X,Y,[H|T]):-
compareCoords(X,Y,H);
checkCoordinates(X,Y,T).

/*Checks if the X coordinates are the unifiable*/
compareCoords(X,Y,[H|T]):-
Y == H,
compareCoords2(X,T).

/*Checks if the Y coordinates are the unifiable*/
compareCoords2(X,[H|T]):- X == H.


checkColour(Colour,[H | [H1|[T1| E]]]):-
T1 == Colour.

countPieces(_,[],_,[]).
countPiecesAdd(_,[],_,[]).

countPiecesColour(Colour, [H|T], length(List2)):-
append([],[],List2),
checkColour(Colour, H) -> countPiecesAdd(Colour, T, H, List2); countPieces(Colour, T, H, List2).

countPiecesAdd(Colour, [H|T], Element, [Element | List2]):-
checkColour(Colour, H) -> countPiecesAdd(Colour, T, H, List2); countPieces(Colour, T, H, List2).

countPieces(Colour, [H|T], Element, List2):-
checkColour(Colour, H) -> countPiecesAdd(Colour, T, H, List2); countPieces(Colour, T, H, List2).

/*Extracts the stone(1/2) of the first element of a list of lists of type [CoordX,CoordY,Stone]*/
extractFirstStone([[E1|[E2|S]]|T], Stone):-
Stone is S.

extractStone(X,Y,[[Y|[X|S]]| T],Stone):-
Stone is S.

extractStone(_,_,[], Stone):-
Stone is 0.

extractStone(X,Y,[[Y1|[X1|S]]| T],Stone):-
extractStone(X,Y,T,Stone).



/* Separação adicionar membros ao Row e rows ao board */
/*Criar um board fazio e fazer inserts*/
boardAfterPlay([Head|Tail], DimX, DimY, NewBoard):-
X = 0, Y = 0,
(checkCoordinates(X,Y,[Head| Tail]) -> (extractFirstStone([Head| Tail], S), addCell(X, Y, Tail, S, Row, DimX, DimY, NewBoard)); (S is 0, addCell(X, Y, [Head| Tail], S , Row, DimX, DimY, NewBoard))).

addCell(X, DimY, [Head| Tail], S,  [S | Row], DimX, DimY, NewBoard).
addCell(DimX, Y, [Head| Tail], S, Row, DimX, DimY, [Row| NewBoard]):- Y1 is (Y + 1), X1 = 0, append([],[],Row1), addCell(X1, Y1, [Head| Tail], S, Row1, DimX, DimY, NewBoard).


addCell(X,Y,[Head| Tail],S,[S | Row], DimX, DimY, NewBoard):-
X1 is X + 1,
(checkCoordinates(X1,Y,[Head| Tail]) -> (extractFirstStone([Head| Tail], S), addCell(X1, Y, Tail, S, Row, DimX, DimY, NewBoard)); (S is 0, addCell(X1, Y, [Head| Tail], S , Row, DimX, DimY, NewBoard))).



makeRowAfter([H|T], DimX, DimX, Y, [], S).

makeRowAfterFirst([H|T], X, DimX, Y, NewRow):-
extractStone(X, Y, [H|T], S1),
makeRowAfter([H|T], X, DimX, Y, NewRow, S1).


makeRowAfter([H|T], X, DimX, Y, [S | NewRow], S):-
X1 is X + 1,
extractStone(X1, Y, [H|T], S1),
makeRowAfter([H|T], X1, DimX, Y, NewRow, S1).



makeBoardAfterFirst([H|T], Y, DimX, DimY, NewBoard):-
X is 0,
makeRowAfterFirst([H|T], X, DimX, Y, NewRow1),
Y1 is Y +1,
makeBoardAfter([H|T], Y1, DimX, DimY, NewBoard, NewRow1).

makeBoardAfter([H|T], DimY, _, DimY, [NewRow| []], NewRow).
makeBoardAfter([H|T], Y, DimX, DimY, [NewRow | NewBoard], NewRow):-
X is 0,
makeRowAfterFirst([H|T], X, DimX, Y, NewRow1),
Y1 is Y +1,
makeBoardAfter([H|T], Y1, DimX, DimY, NewBoard, NewRow1).


completePlay(X1,Y1,X2,Y2,Player,[H|T],NewB):-
makePlay(X1,Y1,X2,Y2,Player, [H|T], NewB1),
removePieces2(X2,Y2,NewB1, List, Player),
sort(List, SortedList),
list_length([H|T], LengthY),
list_length(H, LengthX),
makeBoardAfterFirst(SortedList,0,LengthX,LengthY,NewB).

/*Checks if the cell above has a stone*/
checkUp(X,Y,OldB, NewB,Acc):-
Y1 is Y - 1, 
nth0(Y1, OldB, Row),
nth0(X, Row, Element),
((Element == 1 ; Element == 2), \+checkCoordinates(X,Y1,Acc)) -> removePieces(X,Y1,Element,OldB,NewB,Acc); removePieces(X,Y,Element,[],NewB,Acc).

/*Checks if the cell below has a stone*/
checkDown(X,Y,OldB, NewB,Acc):-
Y1 is Y + 1, 
nth0(Y1, OldB, Row),
nth0(X, Row, Element),
((Element == 1 ; Element == 2), \+checkCoordinates(X,Y1,Acc)) -> removePieces(X,Y1, Element, OldB, NewB,Acc); removePieces(X,Y,Element,[],NewB,Acc).


/*Checks if the cell to the left has a stone*/
checkLeft(X,Y,OldB,NewB,Acc):-
X1 is X - 1, 
nth0(Y, OldB, Row),
nth0(X1, Row, Element),
((Element == 1 ; Element == 2), \+checkCoordinates(X1,Y,Acc)) -> removePieces(X1,Y, Element, OldB, NewB,Acc); removePieces(X,Y,Element,[],NewB,Acc).


/*Checks if the cell to the right has a stone*/
checkRight(X,Y,OldB,NewB,Acc):-
X1 is X + 1, 
nth0(Y, OldB, Row),
nth0(X1, Row, Element),
((Element == 1 ; Element == 2), \+checkCoordinates(X1,Y,Acc))-> removePieces(X1,Y, Element, OldB, NewB,Acc); removePieces(X,Y,Element,[],NewB,Acc).


checkUp2(X,Y,OldB, NewB):-
Y1 is Y - 1, 
nth0(Y1, OldB, Row),
nth0(X, Row, Element),
((Element == 1 ; Element == 2), \+checkCoordinates(X,Y1,[])) -> removePieces(X,Y1,Element,OldB,NewB,[]); removePieces(X,Y,Element,[],NewB,[]).

/*Checks if the cell below has a stone*/
checkDown2(X,Y,OldB, NewB,Acc):-
Y1 is Y + 1, 
nth0(Y1, OldB, Row),
nth0(X, Row, Element),
((Element == 1 ; Element == 2), \+checkCoordinates(X,Y1,Acc)) -> removePieces(X,Y1, Element, OldB, NewB,Acc); removePieces(X,Y,Element,[],NewB,acc).


/*Checks if the cell to the left has a stone*/
checkLeft2(X,Y,OldB,NewB,Acc):-
X1 is X - 1, 
nth0(Y, OldB, Row),
nth0(X1, Row, Element),
((Element == 1 ; Element == 2), \+checkCoordinates(X1,Y,Acc)) -> removePieces(X1,Y, Element, OldB, NewB,Acc); removePieces(X,Y,Element,[],NewB,Acc).


/*Checks if the cell to the right has a stone*/
checkRight2(X,Y,OldB,NewB,Acc):-
X1 is X + 1, 
nth0(Y, OldB, Row),
nth0(X1, Row, Element),
((Element == 1 ; Element == 2), \+checkCoordinates(X1,Y,Acc))-> removePieces(X1,Y, Element, OldB, NewB,Acc); removePieces(X,Y,Element,[],NewB,Acc).

removePieces(_,_,_,[],[],_).

removePieces(0,0,Element,OldB, [[0,0,Element] | NewB],Acc):-
append([[0,0,Element]],Acc,Acc1),
checkDown(0,0,OldB,New2,Acc1),
append(New2,[],New2a), append(New2a, Acc1, Acc2),
checkRight(0,0,OldB,New4,Acc2),
append(New2,New4,NewB1),
sort(NewB1,NewB).

removePieces(6,6,Element,OldB, [[6,6,Element] | NewB],Acc):-
append([[6,6,Element]],Acc,Acc1),
checkUp(6,6,OldB,New2,Acc1),
append(New2,[],New2a), append(New2a, Acc1, Acc2),
checkLeft(6,6,OldB,New4,Acc2),
append(New2,New4,NewB1),
sort(NewB1,NewB).

removePieces(6,0,Element,OldB, [[0,6,Element] | NewB],Acc):-
append([[0,6,Element]],Acc,Acc1),
checkDown(6,0,OldB,New2,Acc1),
append(New2,[],New2a), append(New2a, Acc1, Acc2),
checkLeft(6,0,OldB,New3,Acc2),
append(New2,New3,NewB1),
sort(NewB1,NewB).

removePieces(0,6,Element,OldB, [[6,0,Element] | NewB],Acc):-
append([[6,0,Element]],Acc,Acc1),
checkUp(0,6,OldB,New2,Acc1),
append(New2,[],New2a), append(New2a, Acc1, Acc2),
checkRight(0,6,OldB,New3,Acc2),
append(New2,New3,NewB1),
sort(NewB1,NewB).


removePieces(X,0,Element,OldB, [[0,X,Element] | NewB],Acc):-
append([[0,X,Element]],Acc,Acc1),
checkDown(X,0,OldB,New2,Acc1),
append(New2,[],New2a), append(New2a, Acc1, Acc2),
checkLeft(X,0,OldB,New3,Acc2),
append(New3,[],New3a), append(New3a, Acc2, Acc3),
checkRight(X,0,OldB,New4,Acc3),
append(New2,New3,NewA2),
append(NewA2, New4, NewB1),
sort(NewB1,NewB).


removePieces(X,6,Element,OldB, [[6,X,Element] | NewB],Acc):-
append([[6,X,Element]],Acc,Acc1),
checkUp(X,6,OldB,New2,Acc1),
append(New2,[],New2a), append(New2a, Acc1, Acc2),
checkLeft(X,6,OldB,New3,Acc2),
append(New3,[],New3a), append(New3a, Acc2, Acc3),
checkRight(X,6,OldB,New4,Acc3),
append(New2,New3,NewA2),
append(NewA2, New4, NewB1),
sort(NewB1,NewB).


removePieces(0,Y,Element,OldB, [[Y,0,Element] | NewB],Acc):-
append([[Y,0,Element]],Acc,Acc1),
checkUp(0,Y,OldB,New2,Acc1),
append(New2,[],New2a), append(New2a, Acc1, Acc2),
checkDown(0,Y,OldB,New3,Acc2),
append(New3,[],New3a), append(New3a, Acc2, Acc3),
checkRight(0,Y,OldB,New4,Acc3),
append(New2,New3,NewA2),
append(NewA2, New4, NewB1),
sort(NewB1,NewB).


removePieces(6,Y,Element,OldB, [[Y,6,Element] | NewB],Acc):-
append([[Y,6,Element]],Acc,Acc1),
checkUp(6,Y,OldB,New2,Acc1),
append(New2,[],New2a), append(New2a, Acc1, Acc2),
checkDown(6,Y,OldB,New3,Acc2),
append(New3,[],New3a), append(New3a, Acc2, Acc3),
checkLeft(6,Y,OldB,New4,Acc3),
append(New2,New3,NewA2),
append(NewA2, New4, NewB1),
sort(NewB1,NewB).




removePieces(X, Y, Element, OldB, [[Y,X,Element] | NewB],Acc):-
append([[Y,X,Element]],Acc,Acc1),
checkUp(X,Y,OldB,New1,Acc1),
append(New1,[],New1a),append(Acc1,New1a,Acc2),
checkDown(X,Y,OldB,New2,Acc2),
append(New2,[],New2a),append(Acc2,New2a,Acc3),
checkLeft(X,Y,OldB,New3,Acc3),
append(New3,[],New3a),append(Acc3,New3a,Acc4),
checkRight(X,Y,OldB,New4,Acc4),
append(New1,New2,NewA1),
append(NewA1,New3,NewA2),
append(NewA2, New4, NewB1),
sort(NewB1,NewB).

removePieces2(X, Y, OldB, NewB, Player):-
(Y \= 0 -> checkUp2(X,Y,OldB,New1); 1=1),
append(New1,[],New1a),
(Y \= 6 -> checkDown2(X,Y,OldB,New2,New1a); 1=1),
append(New2,[],New2a),
(X \= 0 -> checkLeft2(X,Y,OldB,New3,New2a); 1=1),
append(New3,[],New3a),
(X \= 6 -> checkRight2(X,Y,OldB,New4,New3a); 1=1),
append(New1,New2,NewA1),
append(NewA1,New3,NewA2),
append(NewA2, New4, NewK),
append(NewK1, [[Y,X,Player]], NewB),
append(NewK, [], NewK1).
/*sort can be used to eliminate duplicate elements and to sort the list*/


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
/*Chooses Computer Play*/
choose_move(Board,Player,Level,[Xs,Ys,Xf,Yf]):-
Level =:= 1,
getRandomPlay(Board,Player,[Xs,Ys,Xf,Yf]).

choose_move(Board,Player,Level,[Xs,Ys,Xf,Yf]):-
Level =:= 2,
getBestPlay(Board,Player,[Xs,Ys,Xf,Yf]).

/*Chooses a Random Play*/
getRandomPlay(Board,Player,[Xs,Ys,Xf,Yf]):-
findall([Xs,Ys,Xf,Yf],validMove([Xs,Ys,Xf,Yf],Board,Player),ListOfMoves),
list_length(ListOfMoves,Size),
random(0,Size,Move),
nth0(Move,ListOfMoves,[Xs,Ys,Xf,Yf]).

/*Chooses Best Play*/
getBestPlay(Board,Player,[Xs,Ys,Xf,Yf]):-
findall([Xs,Ys,Xf,Yf],validMove([Xs,Ys,Xf,Yf],Board,Player),ListOfMoves),
sort(ListOfMoves,NoDupList),
addValueToList(NoDupList,Board,Player,[[]],ValueListOfMoves),
list_length(ValueListOfMoves,Size),
Move is Size - 1,
nth0(Move,ValueListOfMoves,[Val,Xs,Ys,Xf,Yf]).

addValueToList(1,Board,Player,Acc,Acc).
addValueToList([],Board,Player,Acc,ValueListOfMoves):-
sort(Acc,Acc2),
addValueToList(1,Board,Player,Acc2,ValueListOfMoves).

addValueToList([H|T],Board,Player,Acc,ValueListOfMoves):-
nth0(0,H,Xs),
nth0(1,H,Ys),
nth0(2,H,Xf),
nth0(3,H,Yf),
completePlay(Xs,Ys,Xf,Yf,Player,Board,NewBoard),
value(Board,Player,Before),
value(NewBoard,Player,After),
PlayerPieces is Before - After,
EnemyPlayer is (Player mod 2) + 1,
value(Board,EnemyPlayer,EnemyBefore),
value(NewBoard,EnemyPlayer,EnemyAfter),
EnemyPieces is EnemyBefore - EnemyAfter,
Value is PlayerPieces - EnemyPieces,
append([Value],H,NewMove),
append([NewMove],Acc,NewList),
addValueToList(T,Board,Player,NewList,ValueListOfMoves).


/*Calculates the length of a list*/
list_length(Xs,L) :- list_length(Xs,0,L) .

list_length( [], L , L ) .
list_length( [_|Xs] , T , L ) :-
  T1 is T+1 ,
  list_length(Xs,T1,L).

/*Checks if the game is over*/

game_over(Board, 1) :-
	countsPieces(Board, 1, 0, 0).

game_over(Board, 2) :-
	countsPieces(Board, 2, 0, 0).

countsPieces([],Piece,Count,Count).

countsPieces([H|T],Piece,Amount,Acc):-
count_el(H,Counter,Piece,0),
Acc2 is Counter + Acc,
!,
countsPieces(T,Piece,Amount,Acc2).

count_el([], Count,Element, Count).

count_el([H | T], Count,Element, Acc) :-
    H is Element, !,
    Acc2 is Acc + 1,
    count_el(T, Count,Element,Acc2).

count_el([H | T], Count,Element,Acc) :-
    count_el(T, Count,Element,Acc).

/*Avalia*/
value(Board,Player,Value):-
countsPieces(Board,Player,Value,0).

