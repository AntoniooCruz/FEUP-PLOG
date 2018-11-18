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
writeMove(Xs,Ys,Xf,Yf),
(
game_over(NewBoard,Winner) -> print_tab(NewBoard,Size,0), write('Player '), write(Winner) , write(' wins!'); 
NextPlayer is (Player mod 2) + 1,
gameCycle(NewBoard,Size,NextPlayer)    
).


comCycle(Board,Size,Player):-
print_tab(Board,Size,0),nl,
print_info(Board),
writePlayerTurn(Player),
choose_move(Board,Player,1,[Xs,Ys,Xf,Yf]),
completePlay(Xs,Ys,Xf,Yf,Player,Board,NewBoard),
writeMove(Xs,Ys,Xf,Yf),
(
game_over(NewBoard,Winner) -> print_tab(NewBoard,Size,0),write('Player '), write(Winner) , write(' wins!'); 
NextPlayer is (Player mod 2) + 1,
comCycle(NewBoard,Size,NextPlayer) 
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
    choose_move(Board,Player,1,[Xs,Ys,Xf,Yf]),
    completePlay(Xs,Ys,Xf,Yf,Player,Board,NewBoard),
    writeMove(Xs,Ys,Xf,Yf),
    gameVComCycle(NewBoard,Size,1)
    )
).

comVgameCycle(Board,Size,Player):-
print_tab(Board,Size,0),nl,
print_info(Board),
(
    game_over(Board,Winner) -> print_tab(Board,Size,0),write('Player '), write(Winner) , write(' wins!'); 
    (
    Player =:= 2 -> readPlay([Xs,Ys,Xf,Yf],Player,Board),
    completePlay(Xs,Ys,Xf,Yf,Player,Board,NewBoard),
    gameVComCycle(NewBoard,Size,1);

    write('Computer Move: '),nl,
    choose_move(Board,Player,1,[Xs,Ys,Xf,Yf]),
    completePlay(Xs,Ys,Xf,Yf,Player,Board,NewBoard),
    writeMove(Xs,Ys,Xf,Yf),
    gameVComCycle(NewBoard,Size,2)
    )
).





