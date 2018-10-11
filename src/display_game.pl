display_game(T):-
write('| 1 || 2 || 3 || 4 || 5 || 6 || 7 |'),
nl,
write('-----------------------------------'),
nl,
print_tab(T,0).

print_tab([]).
print_tab([L|T],X):-
print_line(L),
C is X+1,
write('| '), write(C) ,write(' |'),nl,
print_tab(T,C).

print_line([]).
print_line([C|L]):-
print_cell(C),
print_line(L).

print_cell(X):- 
traduz(X,V),
write(V).

traduz(0,'|   |').
traduz(1,'| B |').
traduz(2,'| W |').