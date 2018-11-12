:- use_module(library(lists)).
:- use_module(library(random)).

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

/*Verifies if a certain move is valid*/
getPos(X,Y,Element,Board):-
nth0(Y, Board, Row),
nth0(X, Row, Element).

validMove([Xs,Ys,Xf,Yf],Board,Player):-
getPos(Xs,Ys,ElementStart,Board),
getPos(Xf,Yf,ElementEnd,Board),
ElementStart =:= Player,
ElementEnd =\= Player,
ElementEnd =\= 0.

/*Chooses a Random Play*/
getRandomPlay(Board,Player,Play):-
findall([Xs,Ys,Xf,Yf],validMove([Xs,Ys,Xf,Yf],Board,Player),ListOfMoves),
list_length(ListOfMoves,Size),
random(0,Size,Move),
nth0(Move,ListOfMoves,Play).


/*Calculates the length of a list*/
list_length(Xs,L) :- list_length(Xs,0,L) .

list_length( []     , L , L ) .
list_length( [_|Xs] , T , L ) :-
  T1 is T+1 ,
  list_length(Xs,T1,L).
