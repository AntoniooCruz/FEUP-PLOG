
% Gets the input from the user and makes sure its a valid number
% checksNumber(-Number)

checksNumber(Number):-
repeat,
catch(read(Number), 
        error(Err,_Context),
        format('Error in Input! Try Again. ~w\n', [Err])),
number(Number),
Number < 20.

% Asks the user for the size of the Board he wants to play 
% boardSize(-Size)

boardSize(Size):-
write('Choose the size of the board you want to play (Must be an even number below 20): '),
checksNumber(Temp),
(
    Temp mod 2 =\= 0 -> write('Must be an even size'),nl,boardSize(Size);
    Size is Temp 
).

% Reads from the user the play he wants to make and makes sure it is valid
% readPlay(-[Xs,Ys,Xf,Yf],+Player,+Board)

readPlay([Xs,Ys,Xf,Yf],Player,Board):-
writePlayerTurn(Player),
write('Piece that you want to move: '),nl,
write('Column: '),
checksNumber(TempX),
Xst is TempX - 1,
write('Row: '),
checksNumber(TempY),
Yst is TempY - 1,
write('Place to move the piece: '),nl,
write('Column: '),
checksNumber(TempX2),
Xft is TempX2 - 1,
write('Row: '),
checksNumber(TempY2),
Yft is TempY2 - 1,
(
    validMove([Xst,Yst,Xft,Yft],Board,Player) -> write('Valid Move'),nl,
    copy_term(Xst,Xs),copy_term(Yst,Ys),copy_term(Xft,Xf),copy_term(Yft,Yf);
    write('Invalid Move'),nl,nl,readPlay([Xs,Ys,Xf,Yf],Player,Board)
).

% Reads from the user the AI level of Difficulty
% readAILevel(-Level)

readAILevel(Level):-
repeat,
write('AI Difficulty:'),nl,
write('1 - Random '),nl,
write('2 - Smart '),nl,
checksDifficulty(Level).

% Reads and makes sure the level of Difficulty is valid 
% checksDifficulty(-Number)

checksDifficulty(Number):-
repeat,
catch(read(Number), 
        error(Err,_Context),
        format('Error in Input! Try Again. ~w\n', [Err])),
number(Number),
Number > 0,
Number < 3.