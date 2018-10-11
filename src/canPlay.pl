createBoard(X):-
append([[1,0,0],[0,1,0]], [[0,0,1]], X).


vazio(0,0, [[V | W] | T]):- V == 0.
vazio(0,0, [V | W]):- V == 0.
vazio(0,0,V):- V == 0.
vazio(0, Y, [V | W]):- vazio2(0, Y, V).
vazio(X, Y, [V | W]):- X1 is X - 1, vazio(X1, Y, W).
vazio2(0,0, [V | W]):- vazio(0,0, [V | W]).
vazio2(0, Y, [V | W]):- Y1 is Y - 1, vazio2(0, Y1, W).


		  

possoJogar(X,Y,L):-
createBoard(L),
write(L),
X < 3,
Y < 3, 
vazio(X,Y,L).