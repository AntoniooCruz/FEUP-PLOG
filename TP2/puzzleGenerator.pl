:- use_module(library(random)).

seedBoard(Size,SeededBoard):-
Cells is Size * Size,
random(0,Cells,SeedBlack),
random(0,Cells,SeedWhite),
createBoard(Cells,0,SeedBlack,SeedWhite,[],SeededList),
list2LL(SeededList, SeededBoard, Size).

createBoard(Cells,Cells,SeedBlack,SeedWhite,SeededBoard,SeededBoard).

createBoard(Cells,Counter,SeedBlack,SeedWhite,Temp,SeededBoard):-
Counter < Cells,
Counter =\= SeedBlack,
Counter =\= SeedWhite,
append(Temp,[0],NewTemp),
C1 is Counter + 1,
createBoard(Cells,C1,SeedBlack,SeedWhite,NewTemp,SeededBoard).

createBoard(Cells,Counter,SeedBlack,SeedWhite,Temp,SeededBoard):-
Counter < Cells,
Counter =:= SeedWhite,
append(Temp,[1],NewTemp),
C1 is Counter + 1,
createBoard(Cells,C1,SeedBlack,SeedWhite,NewTemp,SeededBoard).

createBoard(Cells,Counter,SeedBlack,SeedWhite,Temp,SeededBoard):-
Counter < Cells,
Counter =:= SeedBlack,
append(Temp,[2],NewTemp),
C1 is Counter + 1,
createBoard(Cells,C1,SeedBlack,SeedWhite,NewTemp,SeededBoard).


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

