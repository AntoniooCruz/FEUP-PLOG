:-ensure_loaded(display).
:-ensure_loaded(puzzleGenerator).

start:-
boardSize(Size),
createBoard(Size,[],Board),
print_tab(Board,Size,0),nl.