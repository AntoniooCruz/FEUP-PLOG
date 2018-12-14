:-use_module(library(clpfd)).

%TODO
%setColumnsSandwich(Board):-

%Sets the "Sandwich"(horizontal orientation) restriction for E1,E2,E3
setLineSandwich([E1,E2,E3|[]]):-
E2 #= E1 #\/ E2 #= E3.

setLineSandwich([E1,E2,E3|T]):-
E2 #= E1 #\/ E2 #= E3,
setLineSandwich([E2,E3|T]).
%--------------------------------------------

%Sets the "Sandwich"(horizontal orientation) restriction for all elements
setLinesSandwich([]).
setLinesSandwich([H|T]):-
setLineSandwich(H),
setLinesSandwich(T).
%------------------------------------------------------------------------

%Sets the restriction that each line must have a length of Dim
setBoardLinesLength([],_).
setBoardLinesLength([H|T], Dim):-
length(H,Dim),
setBoardLinesLength(T,Dim).
%-------------------------------------------------------------


%"Returns" in Sum the value of the sum of pieces in a line
getSumPiecesLine([_],_,0).
getSumPiecesLine([Piece|T], Piece, Sum):-
getSumPiecesLine(T, Piece, Sum1),
Sum #= 1 + Sum1.

getSumPiecesLine([H|T], Piece, Sum):-
getSumPiecesLine(T, Piece, Sum1),
Sum #= Sum1.
%---------------------------------------------------------

%Sets the value of the sum of pieces (of each color) to Value in a line
setSumPiecesLine([],_).
setSumPiecesLine([H|T], Value):-
getSumPiecesLine(H, 1, Sum),
getSumPiecesLine(H, 2, Sum1),
Sum #= Value, Sum1 #= Value,
setSumPiecesLine(T, Value).
%-----------------------------------------------------------------------

%Sets the value of the sum of pieces in each line to Dim/2
setPiecesValuesLines(Board, Dim):-
Value is Dim/2,
setSumPiecesLine(Board,Value).
%---------------------------------------------------------

%TODO
%setPiecesValuesColumns(Board):-

%Sets the restriction that the pieces in coordinates X,Y must be equal in both boards
transferPiece(_,_,0,_).
transferPiece(X,Y,Elem,Final):-
element(Y,Final,Line1),
element(X,Line1,Elem1),
Elem1 #= Elem.
%------------------------------------------------------------------------------------

%"Transfers" the pieces from the initial board to the final one
transferPieceCoord(Initial,Final,-1,Y):-
length(Initial, Dim),
Dim1 is Dim -1,
Y1 is Y -1,
transferPieceCoord(Initial,Final,Dim1,Y1).

transferPieceCoord(Initial,Final,X,Y):-
element(Y,Initial,Line),
element(X,Line,Elem),
transferPiece(X,Y,Elem,Final),
X1 is X - 1,
transferPieceCoord(Initial,Final,X1,Y).
%-------------------------------------------------------------

%Sets restrictions based on the initial board
transferPiecesInitial(Initial, Final):-
length(Initial,Dim),
transferPieceCoord(Initial,Final, Dim, Dim).
%--------------------------------------------


%Sets restrictions for the sum of pieces per Line/Column
setPiecesValues(Board):-
setPiecesValuesLines(Board).
%setPiecesValuesColumns(Board).
%----------------------------------

%Converts a List of lists in a list.
lists2List([],FinalList,FinalList).
lists2List([H|T], Acc, FinalList):-
append(Acc, H, Next),
lists2List(T,Next,FinalList).
%----------------------------------

%Main function sets all the restrictions needed for the board, solves the puzzle
set_board_restrictions(InitialBoard, SolvedBoard):-
SolvedBoard = [[A1,A2,A3,A4],[B1,B2,B3,B4],[C1,C2,C3,C4],[D1,D2,D3,D4]],
length(InitialBoard, BoardDim),
domain(SolvedBoard,1,2),
length(SolvedBoard, BoardDim),
setBoardLinesLength(SolvedBoard, Dim),
setPiecesValues(SolvedBoard, Dim),
labeling([],SolvedBoard).
%--------------------------------------------------------------------------------
