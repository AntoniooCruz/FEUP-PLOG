:-ensure_loaded(input).
:-ensure_loaded(menus).
:-ensure_loaded(display).
:-ensure_loaded(logic).

forms:-
menuDisplay,
getChar(Opt),
processMenuChoice(Opt).

gameCycle(Board,Player):-
print_tab(Board,0),nl,
readPlay([Xs,Ys,Xf,Yf],Player,Board),
completePlay(Xs,Ys,Xf,Yf,Player,Board,NewBoard),
(
    Player =:= 1 -> gameCycle(NewBoard,2);
    gameCycle(NewBoard,1)
).

comCycle(Board,Player):-
print_tab(Board,0),nl,
getRandomPlay(Board,Player,[Xs,Ys,Xf,Yf]),
makePlay([Xs,Ys,Xf,Yf],Player,Board,NewBoard),
(
    Player =:= 1 -> comCycle(NewBoard,2);
    comCycle(NewBoard,1)
).






