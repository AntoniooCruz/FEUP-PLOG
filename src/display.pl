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









print_tab([],Size).
print_tab([],Size,X):-
drawCoordinates(Size),
drawLastLine(Size).

print_tab([L|T],Size,X):-
(
    X =:= 0 -> drawFirstLine(Size); 1 =:= 1
),
Counter is X+1,
print_line(L,Counter,0),
print_tab(T,Size,Counter).

print_line([],LineN,Counter):-
lineDivider(Counter,Counter,LineN).

print_line([Cell|L],LineN,Counter):-
put_code(0x2551),
put_code(0),
print_cell(Cell),
put_code(0),
C1 is Counter + 1,
print_line(L,LineN,C1).

print_cell(X):- 
traduz(X,V),
put_code(V).


traduz(0,0).
traduz(1,0x25CB).
traduz(2,0x25CF).

drawFirstLine(N):- drawFirstLine(N + 1,N + 1).
drawFirstLine(N,1):-
put_code(0x2566),
put_code(0x2550),
put_code(0x2550),
put_code(0x2550),
put_code(0x2557),
nl.

drawFirstLine(N,Counter):-
(
    N =:= Counter -> put_code(0x2554);
    put_code(0x2566)
),
put_code(0x2550),
put_code(0x2550),
put_code(0x2550),
C1 is Counter - 1,
drawFirstLine(N,C1).

lineDivider(N,0,LineN):-
put_code(0x256c),
put_code(0x2550),
put_code(0x2550),
put_code(0x2550),
put_code(0x2563),
nl.


lineDivider(N,Counter,LineN):-
(
    N =:= Counter -> drawCoord(LineN),put_code(0x2551),nl,put_code(0x2560);
    put_code(0x256c)
),
put_code(0x2550),
put_code(0x2550),
put_code(0x2550),
C1 is Counter - 1,
lineDivider(N,C1,LineN).


drawLastLine(N):- drawLastLine(N + 1,N + 1).
drawLastLine(N,1):-
put_code(0x2569),
put_code(0x2550),
put_code(0x2550),
put_code(0x2550),
put_code(0x255d),
nl.

drawLastLine(N,Counter):-
(
    N =:= Counter -> put_code(0x255a);
    put_code(0x2569)
),
put_code(0x2550),
put_code(0x2550),
put_code(0x2550),
C1 is Counter - 1,
drawLastLine(N,C1).

drawCoordinates(Size):-drawCoordinates(Size,1).
drawCoordinates(Size,Size):-
Size < 10,
put_code(0x2551),
put_code(0),
write(Size),
put_code(0),
put_code(0x2551),
put_code(0),
put_code(0),
put_code(0),
put_code(0x2551),
nl.

drawCoordinates(Size):-drawCoordinates(Size,1).
drawCoordinates(Size,Size):-
Size > 9,
put_code(0x2551),
put_code(0),
Code is 55 + Size,
char_code(Text,Code),
write(Text),
put_code(0),
put_code(0x2551),
put_code(0),
put_code(0),
put_code(0),
put_code(0x2551),
nl.

drawCoordinates(Size,Counter):-
Counter < 10,
put_code(0x2551),
put_code(0),
write(Counter),
put_code(0),
C1 is Counter + 1,
drawCoordinates(Size,C1).

drawCoordinates(Size,Counter):-
Counter > 9,
put_code(0x2551),
put_code(0),
Code is 55 + Counter,
char_code(Text,Code),
write(Text),
put_code(0),
C1 is Counter + 1,
drawCoordinates(Size,C1).

drawCoord(LineN):-
LineN > 9,
put_code(0x2551),
put_code(0),
Code is 55 + LineN,
char_code(Text,Code),
write(Text),
put_code(0).

drawCoord(LineN):-
LineN < 10,
put_code(0x2551),
put_code(0),
write(LineN),
put_code(0).

print_info(Board):-
value(Board,1,White),
value(Board,2,Black),
write('White pieces : '),
write(White),nl,
write('Black pieces : '),
write(Black),nl.