:- use_module(library(lists)).
:- use_module(library(random)).

% Aux function for createEmptyBoard
% createEmptyRow(+X,-Row)

createEmptyRow(0, []).
createEmptyRow(X,[0 | Row]):-
X1 is X - 1,
createEmptyRow(X1,Row).

% Aux function for createEmptyBoard
% createEmptyColumns(+Y,+Row,-Board)

createEmptyColumns(0, _, []).
createEmptyColumns(Y, Row, [Row | Board]):-
Y1 is Y - 1,
createEmptyColumns(Y1, Row, Board).

% Creates an empty board with dimensions X*Y
% createEmptyBoard(-Board,+X,+Y)

createEmptyBoard(Board, X, Y):-
createEmptyRow(X,Row),
createEmptyColumns(Y, Row,Board).


% Inserts an element in a list on a given index, returns a list
% insert(+List, +Element, +Index, -NewList)

insert([], Element , 0, [Element | NewList]):- insert([], Element, -1, NewList).
insert([],_,_,[]). 
insert([H|T], Element, 0, [Element | NewList]):- insert([H|T], Element, -3, NewList).
insert([H|T], Element, Index, [H | NewList]):-
Index1 is Index -1,
insert(T, Element, Index1, NewList).

% Creates an example board
% createBoard(-X)

createBoard(X):-
append([[1,0,0],[0,1,0]], [[0,0,1]], X).

% Gets the character in Board[Y][X]
% getPos(+X,+Y,-Element,+Board)

getPos(X,Y,Element,Board):-
nth0(Y, Board, Row),
nth0(X, Row, Element).

% Inserts a stone on a cell, from the board OldB, with coordinates (X,Y)
% insertPiece(+X,+Y,+Player,+OldB,-NewB)

insertPiece(X, Y, Player, OldB, NewB):-
nth0(Y, OldB, Row, RestRows),
nth0(X, Row, _Element, RestOfRow),
insert(RestOfRow, Player, X, NewRow),
insert(RestRows, NewRow, Y, NewB).

% Checks if a play is valid, the player can only move his/her stones to a place with an opponent's stone
% validPlay(+X,+Y,+Player,+Board)

validPlay(X,Y,Player,Board):-
getPos(X,Y,Element,Board),
Element \= Player, Element \= 0.

% Checks for valid play, removes a players stone from one cell and places it in another cell, removes the opponent's stone
% makePlay(+X1,+Y1,+X2,+Y2,+Player,+OldB,-NewB)

makePlay(X1,Y1,X2,Y2,Player,OldB,NewB):-
validPlay(X2,Y2,Player,OldB) -> (insertPiece(X1,Y1,0,OldB,OldB1),
insertPiece(X2,Y2,Player,OldB1,NewB)); (write('Invalid play!'), fail).


% Checks if an element in the list has the coordinates given
% checkCoordinates(+X,+Y,+List)

checkCoordinates(_,_,[]):-fail.
checkCoordinates(X,Y,[H|T]):-
compareCoords(X,Y,H);
checkCoordinates(X,Y,T).

% Checks if the X coordinates are the unifiable
% compareCoords(+X,+Y,+List)

compareCoords(X,Y,[H|T]):-
Y == H,
compareCoords2(X,T).

% Checks if the Y coordinates are the unifiable
% compareCoords2(+X,+List)

compareCoords2(X,[H|_T]):- X == H.

% Returns the colour of the piece in the head of the given list
% checkColour(-Colour, +List)

checkColour(Colour,[_H | [_H1|[T1| _E]]]):-
T1 == Colour.


% Extracts the stone symbol of the first element of a list of lists of type [CoordX,CoordY,Stone]
% extractFirstStone(+List, -Stone)

extractFirstStone([[_E1|[_E2|S]]|_T], Stone):-
Stone is S.

% Extracts the stone symbol of an element of the given list with coordinates (X,Y), if no element is found with the given coordinates returns 0(empty cell)
% extractStone(+X,+Y,+List,-Stone)

extractStone(X,Y,[[Y|[X|S]]| _T],Stone):-
Stone is S.

extractStone(_,_,[], Stone):-
Stone is 0.

extractStone(X,Y,[[_Y1|[_X1|_S]]| T],Stone):-
extractStone(X,Y,T,Stone).


% Creates a row of the board with the stones present after a play was made
% makesRowAfterFirst(+List, +X,+DimX,+Y,-NewRow)

makeRowAfterFirst([H|T], X, DimX, Y, NewRow):-
extractStone(X, Y, [H|T], S1),
makeRowAfter([H|T], X, DimX, Y, NewRow, S1).

% Auxiliary function to makeRowAfterFirst
% makesRowAfter(+List, +X,+DimX,+Y,-NewRow, +S)

makeRowAfter([_H|_T], DimX, DimX, _Y, [], _S).
makeRowAfter([H|T], X, DimX, Y, [S | NewRow], S):-
X1 is X + 1,
extractStone(X1, Y, [H|T], S1),
makeRowAfter([H|T], X1, DimX, Y, NewRow, S1).


% Creates a board with the stones present afte a play was made
%makeBoardAfterFirst(+List,+Y,+DimX,+DimY,-NewBoard)

makeBoardAfterFirst([H|T], Y, DimX, DimY, NewBoard):-
X is 0,
makeRowAfterFirst([H|T], X, DimX, Y, NewRow1),
Y1 is Y +1,
makeBoardAfter([H|T], Y1, DimX, DimY, NewBoard, NewRow1).

% Auxiliary function to makeBoardAfterFirst
%makeBoardAfter(+List,+Y,+DimX,+DimY,-NewBoard,+NewRow)

makeBoardAfter([_H|_T], DimY, _, DimY, [NewRow| []], NewRow).
makeBoardAfter([H|T], Y, DimX, DimY, [NewRow | NewBoard], NewRow):-
X is 0,
makeRowAfterFirst([H|T], X, DimX, Y, NewRow1),
Y1 is Y +1,
makeBoardAfter([H|T], Y1, DimX, DimY, NewBoard, NewRow1).

% Makes a play, that is, checks if the play is valid, moves the player's stone, removes the opponent's stone, checks which stones are connected
% to the moved stone and creates a new board with the correct stone(the moved stone and the ones which are connected to it) 
% move(+X1,+Y1,+X2,+Y2,+Player,+Board,-NewB)

move(X1,Y1,X2,Y2,Player,[H|T],NewB):-
makePlay(X1,Y1,X2,Y2,Player, [H|T], NewB1),
removePieces2(X2,Y2,NewB1, List, Player),
sort(List, SortedList),
list_length([H|T], LengthY),
list_length(H, LengthX),
makeBoardAfterFirst(SortedList,0,LengthX,LengthY,NewB).

% Checks if the cell above has a stone
% checkUp(+X,+Y,+OldB,-NewB,+Acc)

checkUp(X,Y,OldB, NewB,Acc):-
Y1 is Y - 1, 
nth0(Y1, OldB, Row),
nth0(X, Row, Element),
((Element == 1 ; Element == 2), \+checkCoordinates(X,Y1,Acc)) -> removePieces(X,Y1,Element,OldB,NewB,Acc); removePieces(X,Y,Element,[],NewB,Acc).

% Checks if the cell below has a stone
% checkDown(+X,+Y,+OldB,-NewB,+Acc)

checkDown(X,Y,OldB, NewB,Acc):-
Y1 is Y + 1, 
nth0(Y1, OldB, Row),
nth0(X, Row, Element),
((Element == 1 ; Element == 2), \+checkCoordinates(X,Y1,Acc)) -> removePieces(X,Y1, Element, OldB, NewB,Acc); removePieces(X,Y,Element,[],NewB,Acc).


% Checks if the cell to the left has a stone
% checkLeft(+X,+Y,+OldB,-NewB,+Acc)

checkLeft(X,Y,OldB,NewB,Acc):-
X1 is X - 1, 
nth0(Y, OldB, Row),
nth0(X1, Row, Element),
((Element == 1 ; Element == 2), \+checkCoordinates(X1,Y,Acc)) -> removePieces(X1,Y, Element, OldB, NewB,Acc); removePieces(X,Y,Element,[],NewB,Acc).


% Checks if the cell to the right has a stone
% checkRight(+X,+Y,+OldB,-NewB,+Acc)

checkRight(X,Y,OldB,NewB,Acc):-
X1 is X + 1, 
nth0(Y, OldB, Row),
nth0(X1, Row, Element),
((Element == 1 ; Element == 2), \+checkCoordinates(X1,Y,Acc))-> removePieces(X1,Y, Element, OldB, NewB,Acc); removePieces(X,Y,Element,[],NewB,Acc).

% Checks if the cell above has a stone
% checkUp2(+X,+Y,+OldB,-NewB)
checkUp2(_,0,_,_).
checkUp2(X,Y,OldB, NewB):-
Y1 is Y - 1, 
nth0(Y1, OldB, Row),
nth0(X, Row, Element),
((Element == 1 ; Element == 2), \+checkCoordinates(X,Y1,[])) -> removePieces(X,Y1,Element,OldB,NewB,[]); removePieces(X,Y,Element,[],NewB,[]).

% Checks if the cell below has a stone
% checkDown2(+X,+Y,+OldB,-NewB,+Acc)

checkDown2(X,Y,OldB, NewB,Acc):-
Y1 is Y + 1, 
nth0(Y1, OldB, Row),
nth0(X, Row, Element),
((Element == 1 ; Element == 2), \+checkCoordinates(X,Y1,Acc)) -> removePieces(X,Y1, Element, OldB, NewB,Acc); removePieces(X,Y,Element,[],NewB, Acc).


% Checks if the cell to the left has a stone
% checkLeft2(+X,+Y,+OldB,-NewB,+Acc)
checkLeft2(0,_,_,_).
checkLeft2(X,Y,OldB,NewB,Acc):-
X1 is X - 1, 
nth0(Y, OldB, Row),
nth0(X1, Row, Element),
((Element == 1 ; Element == 2), \+checkCoordinates(X1,Y,Acc)) -> removePieces(X1,Y, Element, OldB, NewB,Acc); removePieces(X,Y,Element,[],NewB,Acc).


% Checks if the cell to the right has a stone
% checkRight2(+X,+Y,+OldB,-NewB,+Acc)

checkRight2(X,Y,OldB,NewB,Acc):-
X1 is X + 1, 
nth0(Y, OldB, Row),
nth0(X1, Row, Element),
((Element == 1 ; Element == 2), \+checkCoordinates(X1,Y,Acc))-> removePieces(X1,Y, Element, OldB, NewB,Acc); removePieces(X,Y,Element,[],NewB,Acc).


% Auiliary function to removePieces2
%r removePieces(+X,+Y,+Element,+OldB,-NewB,+Acc)

removePieces(_,_,_,[],[],_).

removePieces(0,0,Element,OldB, [[0,0,Element] | NewB],Acc):-
append([[0,0,Element]],Acc,Acc1),
checkDown(0,0,OldB,New2,Acc1),
append(New2,[],New2a), append(New2a, Acc1, Acc2),
checkRight(0,0,OldB,New4,Acc2),
append(New2,New4,NewB1),
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

% Creates a list with the stones which will not be removed after a play was made
% removePieces2(+X,+Y,+OldB,-NewB,+Player)

removePieces2(X, Y, OldB, NewB, Player):-
checkUp2(X,Y,OldB,New1),
append(New1,[],New1a),
checkDown2(X,Y,OldB,New2,New1a),
append(New2,[],New2a),
checkLeft2(X,Y,OldB,New3,New2a),
append(New3,[],New3a),
checkRight2(X,Y,OldB,New4,New3a),
append(New1,New2,NewA1),
append(NewA1,New3,NewA2),
append(NewA2, New4, NewK),
append(NewK1, [[Y,X,Player]], NewB),
append(NewK, [], NewK1).

% Gets a List with all the valid moves for a certain player in a certain board
% valid_moves(+Board,+Player,-ListOfMoves)

valid_moves(Board,Player,ListOfMoves):-
findall([Xs,Ys,Xf,Yf],validMove([Xs,Ys,Xf,Yf],Board,Player),ListOfMoves).

% Is true when a certain move is valid for a player on a certain Board
% validMove(+[Xs,Ys,Xf,Yf],+Board,+Player)

validMove([Xs,Ys,Xf,Yf],Board,Player):-
getPos(Xs,Ys,ElementStart,Board),
getPos(Xf,Yf,ElementEnd,Board),
ElementStart =:= Player,
ElementEnd =\= Player,
ElementEnd =\= 0.

% Chooses a move to be made by the AI based on the level of difficulty 
% choose_move(+Board,+Player,+Level,-[Xs,Ys,Xf,Yf])

choose_move(Board,Player,Level,[Xs,Ys,Xf,Yf]):-
Level =:= 1,
getRandomPlay(Board,Player,[Xs,Ys,Xf,Yf]).

choose_move(Board,Player,Level,[Xs,Ys,Xf,Yf]):-
Level =:= 2,
getBestPlay(Board,Player,[Xs,Ys,Xf,Yf]).

% Chooses a random valid play 
% getRandomPlay(+Board,+Player,-[Xs,Ys,Xf,Yf])

getRandomPlay(Board,Player,[Xs,Ys,Xf,Yf]):-
valid_moves(Board,Player,ListOfMoves),
list_length(ListOfMoves,Size),
random(0,Size,Move),
nth0(Move,ListOfMoves,[Xs,Ys,Xf,Yf]).

% Chooses based on a greedy algorithm the best possible play
% getBestPlay(+Board,+Player,-[Xs,Ys,Xf,Yf])

getBestPlay(Board,Player,[Xs,Ys,Xf,Yf]):-
valid_moves(Board,Player,ListOfMoves),
sort(ListOfMoves,NoDupList),
addValueToList(NoDupList,Board,Player,[[]],ValueListOfMoves),
list_length(ValueListOfMoves,Size),
Move is Size - 1,
nth0(Move,ValueListOfMoves,[_Val,Xs,Ys,Xf,Yf]).

% Appends to the list of moves the value of a play
% addValueToList(+ListOfMove,+Board,+Player,+Acc,-ValueListOfMoves)

addValueToList(1,_Board,_Player,Acc,Acc).
addValueToList([],Board,Player,Acc,ValueListOfMoves):-
sort(Acc,Acc2),
addValueToList(1,Board,Player,Acc2,ValueListOfMoves).

addValueToList([H|T],Board,Player,Acc,ValueListOfMoves):-
nth0(0,H,Xs),
nth0(1,H,Ys),
nth0(2,H,Xf),
nth0(3,H,Yf),
move(Xs,Ys,Xf,Yf,Player,Board,NewBoard),
value(Board,Player,Before),
value(NewBoard,Player,After),
PlayerPieces is Before - After,
EnemyPlayer is (Player mod 2) + 1,
value(Board,EnemyPlayer,EnemyBefore),
value(NewBoard,EnemyPlayer,EnemyAfter),
enemyPiecesRemoved(EnemyBefore,EnemyAfter,EnemyPieces),
Value is PlayerPieces - EnemyPieces,
append([Value],H,NewMove),
append([NewMove],Acc,NewList),
addValueToList(T,Board,Player,NewList,ValueListOfMoves).

% Calculates the amout of pieces removed from the enemy,
% gives it a high value if the enemy is left with 0 pieces to prevent from making a game losing play 
% enemyPiecesRemoved(+Before,+After,-EnemyPieces)

enemyPiecesRemoved(_Before,After,EnemyPieces):-
After =:= 0,
EnemyPieces is 2000.

enemyPiecesRemoved(Before,After,EnemyPieces):-
After > 0,
EnemyPieces is Before - After.

% Calculates the length of a list
% list_length(+List,-Length)

list_length(Xs,L) :- list_length(Xs,0,L) .

list_length( [], L , L ) .
list_length( [_|Xs] , T , L ) :-
  T1 is T+1 ,
  list_length(Xs,T1,L).

% True if the game is over 
% game_over(+Board,-Winner)

game_over(Board, 1) :-
	countsPieces(Board, 1, 0, 0).

game_over(Board, 2) :-
	countsPieces(Board, 2, 0, 0).

% Counts the amount of pieces of a colour in a certain board 
% countsPieces(+Board,+Piece,-Amount,+Acc)

countsPieces([],_Piece,Count,Count).

countsPieces([H|T],Piece,Amount,Acc):-
count_el(H,Counter,Piece,0),
Acc2 is Counter + Acc,
!,
countsPieces(T,Piece,Amount,Acc2).

% Counts the amount of times a certain element is in a list 
% count_el(+List,-Amount,+Piece,+Acc)

count_el([], Count,_Element, Count).

count_el([H | T], Count,Element, Acc) :-
    H is Element, !,
    Acc2 is Acc + 1,
    count_el(T, Count,Element,Acc2).

count_el([_H | T], Count,Element,Acc) :-
    count_el(T, Count,Element,Acc).

% Gets the value of a board for a certain player 
% value(+Board,+Player,-Value)
value(Board,Player,Value):-
countsPieces(Board,Player,Value,0).



