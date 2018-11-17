
getChar(Opt):-
get_char(Opt),
skip_line.

checksNumber(Number):-
repeat,
catch(read(Number), 
        error(Err,_Context),
        format('Error in Input! Try Again. ~w\n', [Err])),
number(Number),
Number < 20.

boardSize(Size):-
write('Choose the size of the board you want to play (Must be an even number below 20): '),
checksNumber(Temp),
(
    Temp mod 2 =\= 0 -> write('Must be an even size'),nl,boardSize(Size);
    Size is Temp 
).

readPlay([Xs,Ys,Xf,Yf],Player,Board):-
write('Player '),write(Player),nl,
write('Piece that you want to move: '),nl,
write('Collumn: '),
checksNumber(TempX),
Xst is TempX - 1,
write('Row: '),
checksNumber(TempY),
Yst is TempY - 1,
write('Place to move the piece: '),nl,
write('Collumn: '),
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