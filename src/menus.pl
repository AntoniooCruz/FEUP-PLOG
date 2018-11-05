menuDisplay:-
write(' _______________________________________________'),nl,
write('|---------------Welcome to forms!---------------|'),nl,
write('|---------------- 1) P1 vs P2 ------------------|'),nl,
write('|-----------------2) P1 vs COM -----------------|'),nl,
write('|---------------- 3) COM vs COM ----------------|'),nl,
write('|---------------- 4) EXIT! ---------------------|'),nl,
write('|_______________________________________________|'),nl,nl,
read(Response),
processMenuChoice(Response).

PvPGame:-
fail.

processMenuChoice(Response):-
(Response == 1) -> write('P vs P game.');
(Response == 2) -> write('P vs C game.');
(Response == 3) -> write('C vs C game.');
(Response == 4) -> write('exit '), \+fail.



