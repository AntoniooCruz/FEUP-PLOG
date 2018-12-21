:-ensure_loaded(display).

start:-
boardSize(Size),
createBoard(Size,[],Board),
print_tab(Board,Size,0),nl.