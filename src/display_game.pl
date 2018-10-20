display_game(N):-
createTable(N,[]),

print_tab([]).
print_tab([L|T],X):-
print_line(L),
C is X+1,
write(C),nl,
print_tab(T,C).

print_line([]).
print_line([C|L]):-
print_cell(C),
print_line(L).

print_cell(X):- 
traduz(X,V),
write('|'),
put_code(V),
write('|').

createTable(N,L):-
    (
        N mod 2 =\= 0 -> write('The board must have an even length') ; 
        createTableList(N,0,L,F),
        print_tab(F,0)
    ).

createTableList(N,C,T,F):-
    N > 0,
    D is N + C,
    (
        N mod 2 =:= 0 -> createWhiteLine(D,[],F1),
        append([F1],T,T1);
        createBlackLine(D,[],F2),
        append(T,[F2],T1)
    ),
    N1 is N - 1,
    C1 is C + 1,
    createTableList(N1,C1,T1,F).
    createTableList(0,C,T,T).
    

createWhiteLine(D,L,F):-
    D > 0,
    append([1],L,L1),
    D1 is D-1,
    createWhiteLine(D1,L1,F).

createWhiteLine(0,L,L).

createBlackLine(D,L,F):-
    D > 0,
    append([2],L,L1),
    D1 is D-1,
    createBlackLine(D1,L1,F).

createBlackLine(0,L,L).

    


traduz(0,0x25a1).
traduz(1,0x26aa).
traduz(2,0x26ab).

firstLine(N):-
    N > 0,
    put_code(0x2015),
    write(''),
    N1 is N - 1,
    firstLine(N1).
