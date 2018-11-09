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

/*Inserts an element(1) in a list(0) on index(2), returns a list(4)*/
insert([], Element , 0, [Element | NewList]):- insert([], Element, -1, NewList).
insert([],_,_,[]). 
insert([H|T], Element, 0, [Element | NewList]):- insert([H|T], Element, -3, NewList).
insert([H|T], Element, Index, [H | NewList]):-
Index1 is Index -1,
insert(T, Element, Index1, NewList).

createBoard(X):-
append([[1,0,0],[0,1,0]], [[0,0,1]], X).

/*Makes a players play, replaces the cell in coordinates (0,1) with the character(2), on a board(3), returns the resulting board(4)*/
makePlay(X, Y, Player, OldB, NewB):-
nth0(Y, OldB, Row, RestRows),
nth0(X, Row, Element, RestOfRow),
insert(RestOfRow, Player, X, NewRow),
insert(RestRows,NewRow, Y, NewB).

checkUp(X,Y,OldB, NewB):-
Y1 is Y - 1, 
nth0(Y1, OldB, Row),
nth0(X, Row, Element),
Element == 1 -> makePlay(X,Y1,Element,OldB,NewB), removePieces(X,Y1,Element,OldB,NewB);
Element == 2 -> makePlay(X,Y1,Element,OldB,NewB), removePieces(X,Y1,Element,OldB,NewB).

checkDown(X,Y,OldB, NewB):-
Y1 is Y + 1, 
nth0(Y1, OldB, Row),
nth0(X, Row, Element),
Element == 1 -> makePlay(X,Y1,Element,OldB,NewB), removePieces(X,Y1,Element,OldB,NewB);
Element == 2 -> makePlay(X,Y1,Element,OldB,NewB), removePieces(X,Y1,Element,OldB,NewB).

checkLeft(X,Y,OldB,NewB):-
X1 is X - 1, 
nth0(Y, OldB, Row),
nth0(X1, Row, Element),
Element == 1 -> makePlay(X1,Y,Element,OldB,NewB), removePieces(X1,Y,Element,OldB,NewB);
Element == 2 -> makePlay(X1,Y,Element,OldB,NewB), removePieces(X1,Y,Element,OldB,NewB).

checkRight(X,Y,OldB,NewB):-
X1 is X + 1, 
nth0(Y, OldB, Row),
nth0(X1, Row, Element),
Element == 1 -> makePlay(X1,Y,Element,OldB,NewB), removePieces(X1,Y,Element,OldB,NewB);
Element == 2 -> makePlay(X1,Y,Element,OldB,NewB), removePieces(X1,Y,Element,OldB,NewB).


removePieces(X, Y, OldB, NewB):-
makePlay(X, Y, 5, OldB, OldB1),
checkUp(X,Y,OldB1,NewB),
checkDown(X,Y,OldB1,NewB),
checkLeft(X,Y,OldB1,NewB),
checkRight(X,Y,OldB1,NewB),