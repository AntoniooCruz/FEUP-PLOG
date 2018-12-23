%This file will be used to implement restrictions for a List format instead of a List of Lists.

%(!) - modified from restrictions.pl file
%(?) - Needs rework for list format
:-use_module(library(clpfd)).
:-use_module(library(lists)).


%Calls setColumnSandwich with the right arguments
setColumnsSandwich(Board, Dim):-
Stop is Dim -1,
StopF is Stop * Dim,
BeginPos is Dim +1,
setColumnSandwich(Board, BeginPos, StopF, Dim).
%------------------------------------------------

%Sets the Sandwich restriction (vertical orientation) for all board cells(Tests- Success(?))
%Position start = 1, StopF = position of the last cell where the restriction will be applied
setColumnSandwich(Board, StopF, StopF, Dim):-
AntePos is StopF - Dim,
PostPos is StopF + Dim,
element(AntePos, Board, Before),
element(PostPos, Board, After),
element(StopF, Board, Elem),
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


%Sets the "Sandwich"(horizontal orientation) restriction for all board cells(!)(Tests- Success)
%/ Position start = 1 / K = Rest of the division between Cell position and board dimension
setLineSandwich([E1,E2,E3|[]], _, _):-
E2 #= E1 #\/ E2 #= E3.


setLineSandwich([_E1,E2,E3|T], 1, Dim):-
K1 is 2,
Kf is K1 mod Dim,
setLineSandwich([E2,E3|T], Kf, Dim).

setLineSandwich([_E1,E2,E3|T], 0, Dim):-
K1 is 1,
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
setLineSandwich(Board, 2, Dim).
%------------------------------------------------------------------------

%Sets the restriction that each line must have a length of Dim(not used)
setBoardLinesLength([],_).
setBoardLinesLength([H|T], Dim):-
length(H,Dim),
setBoardLinesLength(T,Dim).
%-------------------------------------------------------------

%Sets the Sum of white pieces per line to be equal to be ExpectedSum(3*Dim/2)(Tests - Success) (!)

setSumPiecesLines([H|[]], ActualSum, ExpectedSum, Dim, Dim):-
Sum #= ActualSum + H,
Sum #= ExpectedSum.


setSumPiecesLines([H|T], ActualSum, ExpectedSum, Dim, Dim):-
Sum #= ActualSum + H,
Sum #= ExpectedSum,
setSumPiecesLines(T, 0, ExpectedSum, 1, Dim).

setSumPiecesLines([H|T], ActualSum, ExpectedSum, Pos, Dim):-
NewPos is Pos +1,
Sum #= ActualSum + H,
setSumPiecesLines(T, Sum, ExpectedSum, NewPos, Dim).
%-------------------------------------------------------------------------

%Set the sum of white pieces of each column to be Expected Sum
setSumPiecesColumn(_,_,_,ColumnNum,_,ColumnNum).

setSumPiecesColumn(_, ActualSum, ExpectedSum, ColumnNum, ColumnNum, _):-
ActualSum #= ExpectedSum.

setSumPiecesColumn(Board, ActualSum, ExpectedSum, ColumnNum, 0, LinePos):-
NextPos is LinePos,
nth0(NextPos, Board, Piece),
NextSum #= ActualSum + Piece,
NextColumnPos is 1,
NextLinePos is LinePos + 1,
setSumPiecesColumn(Board, NextSum, ExpectedSum, ColumnNum, NextColumnPos, LinePos),
setSumPiecesColumn(Board, 0, ExpectedSum, ColumnNum, 0, NextLinePos).


setSumPiecesColumn(Board, ActualSum, ExpectedSum, ColumnNum, ColumnPos, LinePos):-
NextPos is LinePos + (ColumnPos * ColumnNum),
nth0(NextPos, Board, Piece),
NextSum #= ActualSum + Piece,
NextColumnPos is ColumnPos + 1,
setSumPiecesColumn(Board, NextSum, ExpectedSum, ColumnNum, NextColumnPos, LinePos).
%----------------------------------------------------------------

%Sets the sum of white pieces for all columns to be Dim/2
setSumPiecesColumns(Board, Dim):-
ExpectedSum is 3 * Dim // 2,
setSumPiecesColumn(Board, 0, ExpectedSum, Dim, 0, 0).
%------------------------------------------------

%"Returns" in Sum the value of the sum of pieces in a line(Not used)
/*getSumPiecesLine([_],_,0, 0).
getSumPiecesLine([Piece|T], Piece, Sum, K):-
getSumPiecesLine(T, Piece, Sum1),
Sum #= 1 + Sum1.

getSumPiecesLine([H|T], Piece, Sum):-
getSumPiecesLine(T, Piece, Sum1),
Sum #= Sum1.
%---------------------------------------------------------

%Sets the value of the sum of pieces (of each color) to Value in a line(Not used)
setSumPiecesLine([_],_,0).
setSumPiecesLine([H|T], Value,Dim):-
getSumPiecesLine(H, 1, Sum),
getSumPiecesLine(H, 2, Sum1),
Sum #= Value, Sum1 #= Value,
setSumPiecesLine(T, Value).*/
%-----------------------------------------------------------------------

%Sets the value of the sum of pieces in each line to Dim/2
setPiecesValuesLines(Board, Dim):-
Value is Dim // 2,
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


equal(LL,LL).

%Converts a List to a List of lists
list2LL(List, LL, Dim):-
l2ll(List, LL,[], Dim, [], 0).


l2ll([], LL, Aux, _Dim, TempList, _):-
append(Aux, [TempList], NewAux),
equal(LL, NewAux).


l2ll([H|T], LL, Aux, Dim, TempList, Dim):-
append(Aux, [TempList], NewAux),
l2ll([H|T], LL, NewAux, Dim, [], 0).

l2ll([H|T], LL, Aux, Dim, TempList, Pos):-
append(TempList,[H], NewList),
NewPos is Pos + 1,
l2ll(T, LL , Aux, Dim, NewList, NewPos).



%Main function sets all the restrictions needed for the board, solves the puzzle
set_board_restrictions(InitialBoard, SolvedBoardList,Solution):-
lists2List(InitialBoard,[],InitialList),
length(InitialBoard, BoardDim),
length(InitialList,NumCells),
length(SolvedBoardList, NumCells),
domain(SolvedBoardList,1,2), %Used in Sicstus
%SolvedBoardList ins 1..2, % Used in SWI-Prolog
transferPieceCoord(InitialList,SolvedBoardList),
ExpectedSum is 3 * BoardDim // 2,
setLinesSandwich(SolvedBoardList, BoardDim),
setColumnsSandwich(SolvedBoardList, BoardDim),
setSumPiecesLines(SolvedBoardList, 0, ExpectedSum, 1, BoardDim),
setSumPiecesColumns(SolvedBoardList, BoardDim),
labeling([],SolvedBoardList),
list2LL(SolvedBoardList, Solution, BoardDim).
%--------------------------------------------------------------------------------
