:- use_module(library(lists)).

create_game(N):-
    createBoard(N,[],FinalBoard),
    display_game(FinalBoard,Player).

display_game(Board,Player):-
    print_tab(Board,0).
    
createBoard(N,Board,FinalBoard):-
    (
        N mod 2 =\= 0 -> write('The board must have an even length'),
        createBoard(0,0,[],[]) ; 
        createBoardList(N,0,Board,FinalBoard),
        createBoard(0,0,FinalBoard,FinalBoard)
    ).
createBoard(0,0,FinalBoard,FinalBoard).

createBoardList(N,Counter,Board,FinalBoard):-
    N > 0,
    Size is N + Counter,
    (
        N mod 2 =:= 0 -> createWhiteLine(Size,[],F1),
        append([F1],Board,B1);
        createBlackLine(Size,[],B2),
        append(Board,[B2],B1)
    ),
    N1 is N - 1,
    C1 is Counter + 1,
    createBoardList(N1,C1,B1,FinalBoard).
    createBoardList(0,Counter,Board,Board).
    

createWhiteLine(Size,Line,F):-
    Size > 0,
    append([1],Line,L1),
    S1 is Size-1,
    createWhiteLine(S1,L1,F).

createWhiteLine(0,Line,Line).

createBlackLine(Size,Line,F):-
    Size > 0,
    append([2],Line,L1),
    S1 is Size-1,
    createBlackLine(S1,L1,F).

createBlackLine(0,Line,Line).

print_tab([]).
print_tab([],0).
print_tab([L|T],X):-
Counter is X+1,
print_line(L),
write(Counter),nl,
print_tab(T,Counter).

print_line([]).
print_line([Counter|L]):-
print_cell(Counter),
print_line(L).

print_cell(X):- 
traduz(X,V),
write('|'),
put_code(V),
write('|').


traduz(0,0x2715).
traduz(1,0x26aa).
traduz(2,0x26ab).

firstLine(N,Counter):-
    N > 0,
    C1 is Counter + 1,
    write(C1),
    write(' |'),
    N1 is N - 1,
    firstLine(N1,C1).


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
insertPiece(X2,Y2,Player,OldB1,NewB)); write('Invalid play!').

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



