:-use_module(library(clpfd)).

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

getSumPiecesLine(Line, Piece)

setPiecesValuesLines(Board):-

setPiecesValuesColumns(Board):-

setPiecesValues(Board):-
setPiecesValuesLines(Board),
setPiecesValuesColumns(Board).

set_board_restrictions(InicialBoard, SolvedBoard):-
length(InicialBoard, BoardDim),
domain(SolvedBoard,1,2),
length(SolvedBoard, BoardDim),
setBoardLinesLength(SolvedBoard, Dim),
setPiecesValues(SolvedBoard),
labeling([],SolvedBoard).
