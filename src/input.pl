
getChar(Opt):-
get_char(Opt),
skip_line.

boardSize(Size):-
write('Choose the size of the board you want to play (Must be an even number): '),
getChar(SizeChar),
number_chars(Temp,[SizeChar]),
(
    Temp mod 2 =\= 0 -> write('Must be an even size'),nl,boardSize(Size);
    Size is Temp 
).

readPlay([Xs,Ys,Xf,Yf],Player,Board):-
write('Player '),write(Player),nl,
write('Piece that you want to move: '),nl,
write('Collumn: '),
getChar(XsChar),
number_chars(TempX,[XsChar]),nl,
Xst is TempX - 1,
write('Row: '),
getChar(YsChar),
number_chars(TempY,[YsChar]),nl,
Yst is TempY - 1,
write('Place to move the piece: '),nl,
write('Collumn: '),
getChar(XfChar),
number_chars(TempX2,[XfChar]),nl,
Xft is TempX2 - 1,
write('Row: '),
getChar(YfChar),
number_chars(TempY2,[YfChar]),nl,
Yft is TempY2 - 1,
(
    validMove([Xst,Yst,Xft,Yft],Board,Player) -> write('Valid Move'),nl,
    copy_term(Xst,Xs),copy_term(Yst,Ys),copy_term(Xft,Xf),copy_term(Yft,Yf);
    write('Invalid Move'),nl,nl,readPlay([Xs,Ys,Xf,Yf],Player,Board)
).