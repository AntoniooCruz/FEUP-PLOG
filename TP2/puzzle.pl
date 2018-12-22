:-ensure_loaded(display).
:-ensure_loaded(puzzleGenerator).
:-ensure_loaded(restrictions2).

start:-
boardSize(Size),
seedBoard(Size,Board),
set_board_restrictions(Board, LineSolution,FinalBoard),
generatePuzzle(FinalBoard,BoardToSolve),
print_tab(BoardToSolve,Size,0),
set_board_restrictions(BoardToSolve, LineSolution,FinalBoard),
print_tab(FinalBoard,Size,0).
