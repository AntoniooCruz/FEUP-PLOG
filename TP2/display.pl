% Displays the Board on the screen
% print_tab(+Board,+Size)

print_tab([],_Size).
print_tab([],Size,_X):-
drawCoordinates(Size),
drawLastLine(Size).

print_tab([L|T],Size,X):-
X =:= 0,
drawFirstLine(Size),
Counter is X+1,
print_line(L,Counter,0),
print_tab(T,Size,Counter).

print_tab([L|T],Size,X):-
Counter is X+1,
print_line(L,Counter,0),
print_tab(T,Size,Counter).

% Auxiliar function to the print_tab, displays a line of the board
% print_line(+Board,+LineN,+Counter)

print_line([],LineN,Counter):-
lineDivider(Counter,Counter,LineN).

print_line([Cell|L],LineN,Counter):-
put_code(0x2551),
put_code(0),
print_cell(Cell),
put_code(0),
C1 is Counter + 1,
print_line(L,LineN,C1).

% Auxiliar function to the print_tab, prints a single Cell
% print_cell(+Cell)

print_cell(X):- 
translates(X,V),
put_code(V).

% Attributes the uni-code char to the different elements
% translates(+Cell,-UniCode)

translates(0,0).
translates(1,0x25CB).
translates(2,0x25CF).

% Draws the first line of the board
% drawFirstLine(+SizeOfBoard)

drawFirstLine(N):- drawFirstLine(N + 1,N + 1).
drawFirstLine(_N,1):-
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

% Draws the the division between each line
% lineDivider(+Size,+Counter,+LineN)

lineDivider(_N,0,_LineN):-
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

% Draws the last line of the board
% drawLastLine(+SizeOfBoard)

drawLastLine(N):- drawLastLine(N + 1,N + 1).
drawLastLine(_N,1):-
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

% Writes the Coordinates of the board horizontally
% drawCoordinates(+Size)

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

% Writes the Coordinates of the board vertically
% drawCoord(+LineN)

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


% Asks the user for the size of the Board he wants to play 
% boardSize(-Size)

boardSize(Size):-
write('Choose the size of the board you want to play (Must be an even number below 20): '),
checksNumber(Temp),
(
    Temp mod 2 =\= 0 -> write('Must be an even size'),nl,boardSize(Size);
    Size is Temp 
).

% Gets the input from the user and makes sure its a valid number
% checksNumber(-Number)

checksNumber(Number):-
repeat,
catch(read(Number), 
        error(Err,_Context),
        format('Error in Input! Try Again. ~w\n', [Err])),
number(Number),
Number < 20.