:- use_module(library(random)).

% Creates a Board dynamically given a certain Size
% createBoard(+Size,+InitialEmptyBoard,-FinalBoard)

createBoard(0,0,FinalBoard,FinalBoard).

createBoard(N,_Board,_FinalBoard):-
N mod 2 =\= 0,
write('The board must have an even length'),
createBoard(0,0,[],[]).

createBoard(N,Board,FinalBoard):-
N mod 2 =:= 0,
createBoardList(N,0,Board,FinalBoard),
createBoard(0,0,FinalBoard,FinalBoard).

% Auxiliar function to the createBoard
% createBoardList(+Size,+Counter,+Board,-FinalBoard)

createBoardList(N,Counter,Board,FinalBoard):-
N > 0,
Size is N + Counter,
createLine(Size,[],F1),
append([F1],Board,B1),
N1 is N - 1,
C1 is Counter + 1,
createBoardList(N1,C1,B1,FinalBoard).
createBoardList(0,_Counter,Board,Board).

% Creates a line filled with pieces
% createLine(+Size,+Line,-FinalLine)

createLine(Size,Line,F):-
    Size > 0,
    append([0],Line,L1),
    S1 is Size-1,
    createLine(S1,L1,F).

createLine(0,Line,Line).

generatePuzzle(SolvableBoard,BoardToSolve):-
lists2List(SolvableBoard,[],SolvableList),
length(SolvableBoard, BoardDim),
makePuzzle(SolvableList,BoardList,[],1,BoardDim,BoardToSolve).

makePuzzle([],BoardList,OldList,Skip,BoardDim,BoardToSolve):-
list2LL(OldList, BoardToSolve, BoardDim).

makePuzzle([H | T],BoardList,OldList,Skip,BoardDim,BoardToSolve):-
Skip =\= 0,
append(OldList,[0],BoardList),
random(0,4,NewSkip),
makePuzzle(T,New,BoardList,NewSkip,BoardDim,BoardToSolve).

makePuzzle([H | T],BoardList,OldList,Skip,BoardDim,BoardToSolve):-
Skip =:= 0,
append(OldList,[H],BoardList),
random(0,4,NewSkip),
makePuzzle(T,New,BoardList,NewSkip,BoardDim,BoardToSolve).

