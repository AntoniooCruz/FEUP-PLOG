:-ensure_loaded(input).
:-ensure_loaded(menus).
:-ensure_loaded(display).
:-ensure_loaded(logic).

forms:-
menuDisplay,
getChar(Opt),
processMenuChoice(Opt).

gameCycle(Board,Size,Player):-
print_tab(Board,Size,0),nl,
readPlay([Xs,Ys,Xf,Yf],Player,Board),
completePlay(Xs,Ys,Xf,Yf,Player,Board,NewBoard),
(
    Player =:= 1 -> gameCycle(NewBoard,Size,2);
    gameCycle(NewBoard,Size,1)
).

comCycle(Board,Size,Player):-
print_tab(Board,Size,0),nl,
getRandomPlay(Board,Player,[Xs,Ys,Xf,Yf]),
makePlay([Xs,Ys,Xf,Yf],Player,Board,NewBoard),
(
    Player =:= 1 -> comCycle(NewBoard,2);
    comCycle(NewBoard,Size,1)
).






