:- use_module(library(lists)).

createBoard(X):-
append([[1,0,0],[0,1,0]], [[0,0,1]], X).

validCondition(J, X):-
J == 1 -> X == 0 ; X == 2.
J == 2 -> X == 0 ; X == 1.


vazio(0,0, [[V | W] | T]):- V == 0.
vazio(0,0, [V | W]):- V == 0.
vazio(0,0,V):- V == 0.
vazio(0, Y, [V | W]):- vazio2(0, Y, V).
vazio(X, Y, [V | W]):- X1 is X - 1, vazio(X1, Y, W).
vazio2(0,0, [V | W]):- vazio(0,0, [V | W]).
vazio2(0, Y, [V | W]):- Y1 is Y - 1, vazio2(0, Y1, W).




selectRow(0, [V | W]) :- 0 == 0.
selectRow(Y, [V | W]) :- Y1 is Y - 1, selectRow(Y1, W). 






/* gets the elements of a list that are before a certain element*/
getB(X):-
createBoard(X),
getBefore(X, [V | W], L1).

getBefore(0,L,L1):- append(L1,[],L1).
getBefore(X,[V | W],L1):-
append(L1, V, L2),
X1 is X - 1,
getBefore(X1, W, L2). 

/* gets the elements of a list that are after a certain element*/



		  

possoJogar(X,Y,L):-
createBoard(L),
write(L),
X < 3,
Y < 3, 
vazio(X,Y,L).