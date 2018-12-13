:-use_module(library(clpfd)).

%TODO
setColumnsSandwich(Board):-

setLineSandwich([E1,E2,E3|[]]):-
E2 #= E1 #\/ E2 #= E3.

setLineSandwich([E1,E2,E3|T]):-
E2 #= E1 #\/ E2 #= E3,
setLineSandwich([E2,E3|T]).

setLinesSandwich([]).
setLinesSandwich([H|T]):-
setLineSandwich(H),
setLinesSandwich(T).

setBoardLinesLength([],_).
setBoardLinesLength([H|T], Dim):-
length(H,Dim),
setBoardLinesLength(T,Dim).


getSumPiecesLine([_],_,0).
getSumPiecesLine([Piece|T], Piece, Sum):-
getSumPiecesLine(T, Piece, Sum1),
Sum #= 1 + Sum1.

getSumPiecesLine([H|T], Piece, Sum):-
getSumPiecesLine(T, Piece, Sum1),
Sum #= Sum1.

setSumPiecesLine([],_).
setSumPiecesLine([H|T], Value):-
getSumPiecesLine(H, 1, Sum),
getSumPiecesLine(H, 2, Sum1),
Sum #= Value, Sum1 #= Value,
setSumPiecesLine(T, Value).

setPiecesValuesLines(Board, Dim):-
Value is Dim/2,
setSumPiecesLine(Board,Value).

%TODO
setPiecesValuesColumns(Board):-

transferPiece(_,_,0,_).
transferPiece(X,Y,Elem,Final):-
element(Y,Final,Line1),
element(X,Line1,Elem1),
Elem1 #= Elem.

transferPieceCoord(Initial,Final,-1,Y):-
length(Initial, Dim),
Dim1 is Dim -1
Y1 is Y -1,
transferPieceCoord(Initial,Final,Dim1,Y1).

transferPieceCoord(Initial,Final,X,Y):-
element(Y,Initial,Line),
element(X,Line,Elem),
transferPiece(X,Y,Elem,Final),
X1 is X - 1,
transferPieceCoord(Initial,Final,X1,Y).


transferPiecesInitial(Initial, Final):-
length(Initial,Dim),
transferPieceCoord(Initial,Final, Dim, Dim).

setPiecesValues(Board):-
setPiecesValuesLines(Board),
setPiecesValuesColumns(Board).

set_board_restrictions(InitialBoard, SolvedBoard):-
length(InitialBoard, BoardDim),
domain(SolvedBoard,1,2),
length(SolvedBoard, BoardDim),
setBoardLinesLength(SolvedBoard, Dim),
setPiecesValues(SolvedBoard, Dim),
labeling([],SolvedBoard).
