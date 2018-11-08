
insert2([], _ , _ , []).
insert2([H|T], Element, 0, [Element | NewList]):- insert2([H|T], Element, -1, NewList).
insert2([H|T], Element, Index, [H | NewList]):- 
Index >= length([H|T]) -> append(NewList, [Element], NewList1), NewList is NewList1;
Index1 is Index -1,
insert2(T, Element, Index1, NewList).
