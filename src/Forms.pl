:-ensure_loaded(input).
:-ensure_loaded(menus).
:-ensure_loaded(display).
:-ensure_loaded(logic).

forms:-
menuDisplay,
checksNumber(Opt),
processMenuChoice(Opt).

gameCycle(Board,Size,Player):-
print_tab(Board,Size,0),nl,
print_info(Board),
readPlay([Xs,Ys,Xf,Yf],Player,Board),
completePlay(Xs,Ys,Xf,Yf,Player,Board,NewBoard),
(
    game_over(NewBoard,Winner) -> print_tab(NewBoard,Size,0), write('Player '), write(Winner) , write(' wins!'); 
    (
    Player =:= 1 -> gameCycle(NewBoard,Size,2);
    gameCycle(NewBoard,Size,1)
    )
).

comCycle(Board,Size,Player):-
print_tab(Board,Size,0),nl,
print_info(Board),
write('Player '),write(Player),write(' turn'),nl,
getRandomPlay(Board,Player,[Xs,Ys,Xf,Yf]),
completePlay(Xs,Ys,Xf,Yf,Player,Board,NewBoard),
(
    game_over(NewBoard,Winner) -> print_tab(NewBoard,Size,0),write('Player '), write(Winner) , write(' wins!'); 
    (
    Player =:= 1 -> comCycle(NewBoard,Size,2);
    comCycle(NewBoard,Size,1)
    )
).

gameVComCycle(Board,Size,Player):-
print_tab(Board,Size,0),nl,
print_info(Board),
(
    game_over(Board,Winner) -> print_tab(Board,Size,0),write('Player '), write(Winner) , write(' wins!'); 
    (
    Player =:= 1 -> readPlay([Xs,Ys,Xf,Yf],Player,Board),
    completePlay(Xs,Ys,Xf,Yf,Player,Board,NewBoard),
    gameVComCycle(NewBoard,Size,2);

    write('Computer Move: '),nl,
    getRandomPlay(Board,Player,[Xs,Ys,Xf,Yf]),
    completePlay(Xs,Ys,Xf,Yf,Player,Board,NewBoard),
    gameVComCycle(NewBoard,Size,1)
    )
).






