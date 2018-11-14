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
print_tab([],X).
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


traduz(0,0).
traduz(1,0x25CB).
traduz(2,0x25CF).

firstLine(N,Counter):-
    N > 0,
    C1 is Counter + 1,
    write(C1),
    write(' |'),
    N1 is N - 1,
    firstLine(N1,C1).
