:-ensure_loaded(input).
:-ensure_loaded(menus).
:-ensure_loaded(display).
:-ensure_loaded(logic).

% Starts the game by displaying the main menu 

play:-
menuDisplay,
checksNumber(Opt),
processMenuChoice(Opt).

% Game Cycle for the Player vs Player option 
% gameCycle(+Board,+Size,+Player)

gameCycle(Board,Size,Player):-
print_tab(Board,Size,0),nl,
print_info(Board),
readPlay([Xs,Ys,Xf,Yf],Player,Board),
move(Xs,Ys,Xf,Yf,Player,Board,NewBoard),
writeMove(Xs,Ys,Xf,Yf),
(
game_over(NewBoard,Winner) -> print_tab(NewBoard,Size,0),writeWinner(Winner); 
NextPlayer is (Player mod 2) + 1,
gameCycle(NewBoard,Size,NextPlayer)    
).

% Game Cycle for the Com vs Com option
% comCycle(+Board,+Size,+Player,+Level)

comCycle(Board,Size,Player,Level):-
print_tab(Board,Size,0),nl,
print_info(Board),
writePlayerTurn(Player),
choose_move(Board,Player,Level,[Xs,Ys,Xf,Yf]),
move(Xs,Ys,Xf,Yf,Player,Board,NewBoard),
writeMove(Xs,Ys,Xf,Yf),
(
game_over(NewBoard,Winner) -> print_tab(NewBoard,Size,0),writeWinner(Winner); 
NextPlayer is (Player mod 2) + 1,
comCycle(NewBoard,Size,NextPlayer,Level) 
).

% Game Cycle for the Player vs Com option 
% gameVComCycle(+Board,+Size,+Player,+Level)

gameVComCycle(Board,Size,Player,Level):-
print_tab(Board,Size,0),nl,
print_info(Board),
(
    game_over(Board,Winner) -> print_tab(Board,Size,0),writeWinner(Winner); 
    (
    Player =:= 1 -> readPlay([Xs,Ys,Xf,Yf],Player,Board),
    move(Xs,Ys,Xf,Yf,Player,Board,NewBoard),
    gameVComCycle(NewBoard,Size,2,Level);

    write('Computer Move: '),nl,
    choose_move(Board,Player,Level,[Xs,Ys,Xf,Yf]),
    move(Xs,Ys,Xf,Yf,Player,Board,NewBoard),
    writeMove(Xs,Ys,Xf,Yf),
    gameVComCycle(NewBoard,Size,1,Level)
    )
).

% Game Cycle for the Com vs Player option
% comVgameCycle(+Board,+Size,+Player,+Level)

comVgameCycle(Board,Size,Player,Level):-
print_tab(Board,Size,0),nl,
print_info(Board),
(
    game_over(Board,Winner) -> print_tab(Board,Size,0),writeWinner(Winner); 
    (
    Player =:= 2 -> readPlay([Xs,Ys,Xf,Yf],Player,Board),
    move(Xs,Ys,Xf,Yf,Player,Board,NewBoard),
    comVgameCycle(NewBoard,Size,1,Level);

    write('Computer Move: '),nl,
    choose_move(Board,Player,Level,[Xs,Ys,Xf,Yf]),
    move(Xs,Ys,Xf,Yf,Player,Board,NewBoard),
    writeMove(Xs,Ys,Xf,Yf),
    comVgameCycle(NewBoard,Size,2,Level)
    )
).





