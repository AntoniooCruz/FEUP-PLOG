%This file will be used to implement restrictions for a List format instead of a List of Lists.

%(!) - modified from restrictions.pl file
%(?) - Needs rework for list format
:-use_module(library(clpfd)).

%Calls setColumnSandwich with the right arguments
setColumnsSandwich(Board, Dim):-
Stop is Dim -1,
StopF is Stop * Dim,
BeginPos is Dim +1,
setColumnSandwich(Board, BeginPos, StopF, Dim).
%------------------------------------------------

%Sets the Sandwich restriction (vertical orientation) for all board cells
%Position start = 1, StopF = position of the last cell where the restriction will be applied
setColumnSandwich(Board, StopF, StopF, Dim):-
AntePos is Pos - Dim,
PostPos is Pos + Dim,
element(AntePos, Board, Before),
element(PostPos, Board, After),
element(Pos, Board, Elem),
Elem #= Before #\/ Elem #= After.

setColumnSandwich(Board, Pos, StopF, Dim):-
AntePos is Pos - Dim,
PostPos is Pos + Dim,
element(AntePos, Board, Before),
element(PostPos, Board, After),
element(Pos, Board, Elem),
Elem #= Before #\/ Elem #= After,
NextPos is Pos + 1,
setColumnSandwich(Board, NextPos, StopF, Dim).
%---------------------------------------------------------------------------------------------


%Sets the "Sandwich"(horizontal orientation) restriction for all board cells(!)
%/ Position start = 1 / K = Rest of the division between Cell position and board dimension
setLineSandwich([E1,E2,E3|[]], _, _):-
E2 #= E1 #\/ E2 #= E3.


setLineSandwich([E1,E2,E3|T], 1, Dim):-
K1 is K + 1,
Kf is K1 mod Dim,
setLineSandwich([E2,E3|T], Kf, Dim).

setLineSandwich([E1,E2,E3|T], 0, Dim):-
K1 is K + 1,
Kf is K1 mod Dim,
setLineSandwich([E2,E3|T], Kf, Dim).

setLineSandwich([E1,E2,E3|T], K, Dim):-
E2 #= E1 #\/ E2 #= E3,
K1 is K + 1,
Kf is K1 mod Dim,
setLineSandwich([E2,E3|T], Kf, Dim).
%-----------------------------------------------------------------------------------------

%Sets the "Sandwich"(horizontal orientation) restriction for all elements(!)
setLinesSandwich(Board, Dim):-
setLineSandwich(Board, 1, Dim).
%------------------------------------------------------------------------

%Sets the restriction that each line must have a length of Dim(not used)
setBoardLinesLength([],_).
setBoardLinesLength([H|T], Dim):-
length(H,Dim),
setBoardLinesLength(T,Dim).
%-------------------------------------------------------------

%Sets the Sum of pieces per line to be equal(WIP!!!)
setSumPiecesLines([1|T], ActualSum, ExpectedSum, Dim, Dim):-
NewSum #= ActualSum + 1,
NewSum #= ExpectedSum,
setSumPiecesLines(T, 0, ExpectedSum, 0, Dim).


setSumPiecesLines([1|T], ActualSum, ExpectedSum, Pos, Dim):-
NewSum #= ActualSum + 1,
NewPos is Pos +1,
setSumPiecesLines(T, NewSum, ExpectedSum, NewPos, Dim).

setSumPiecesLines([H|T], ActualSum, ExpectedSum, Pos, Dim):-
NewPos is Pos +1,
setSumPiecesLines(T, ActualSum, ExpectedSum, NewPos, Dim).
%--------------------------------------------

%"Returns" in Sum the value of the sum of pieces in a line(?)
getSumPiecesLine([_],_,0, 0).
getSumPiecesLine([Piece|T], Piece, Sum, K):-
getSumPiecesLine(T, Piece, Sum1),
Sum #= 1 + Sum1.

getSumPiecesLine([H|T], Piece, Sum):-
getSumPiecesLine(T, Piece, Sum1),
Sum #= Sum1.
%---------------------------------------------------------

%Sets the value of the sum of pieces (of each color) to Value in a line(?)
setSumPiecesLine([_],_,0).
setSumPiecesLine([H|T], Value,Dim):-
getSumPiecesLine(H, 1, Sum),
getSumPiecesLine(H, 2, Sum1),
Sum #= Value, Sum1 #= Value,
setSumPiecesLine(T, Value).
%-----------------------------------------------------------------------

%Sets the value of the sum of pieces in each line to Dim/2
setPiecesValuesLines(Board, Dim):-
Value is Dim/2,
setSumPiecesLine(Board,Value, Dim).
%---------------------------------------------------------

%TODO
%setPiecesValuesColumns(Board):-

%Sets the restriction that the pieces in coordinates X,Y must be equal in both boards (!)
transferPiece(0,_).
transferPiece(Hi,Hf):-
Hf #= Hi.
%------------------------------------------------------------------------------------

%"Transfers" the pieces from the initial board to the final one (!)
transferPieceCoord([],[]).
transferPieceCoord([Hi|Ti],[Hf|Tf]):-
transferPiece(Hi,Hf),
transferPieceCoord(Ti,Tf).

%-------------------------------------------------------------

%Sets restrictions based on the initial board (Not used)
transferPiecesInitial(Initial, Final):-
length(Initial,Dim),
transferPieceCoord(Initial,Final, Dim, Dim).
%--------------------------------------------


%Sets restrictions for the sum of pieces per Line/Column
setPiecesValues(Board,Dim):-
setPiecesValuesLines(Board,Dim).
%setPiecesValuesColumns(Board,Dim).
%-------------------------------------------------------

%Converts a List of lists in a list.
lists2List([],FinalList,FinalList).
lists2List([H|T], Acc, FinalList):-
append(Acc, H, Next),
lists2List(T,Next,FinalList).
%----------------------------------

%Main function sets all the restrictions needed for the board, solves the puzzle
set_board_restrictions(InitialBoard, SolvedBoard):-
lists2List(InitialBoard,[],InitialList),
length(InitialBoard, BoardDim),
length(InitialList,NumCells),
length(SolvedBoard, NumCells),
%domain(SolvedBoard,1,2), %Used in Sicstus
SolvedBoard ins 1..2, % Used in SWI-Prolog
setPiecesValues(SolvedBoard, Dim),
labeling([],SolvedBoard).
%--------------------------------------------------------------------------------

test(I,F):-
length(I,Dim),
length(F,Dim),
transferPieceCoord(I,F),
labeling([],F).
%_MISC_
%SolvedBoard = [[A1,A2,A3,A4],[B1,B2,B3,B4],[C1,C2,C3,C4],[D1,D2,D3,D4]],

