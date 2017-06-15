
reverse(List,List2):-
  reverse2(List,List2,[]).
reverse2([],Z,Z).
reverse2([H|T],Z,Acc):-
  reverse2(T,Z,[H|Acc]).

lengthList([],0).
lengthList([_|T],N):-
  lengthList(T,N2),
  N is N2 + 1.

substituteIdx(OriginalItem,List,ReplacedItem,List2,IndexItemReplace):-
  substituteIdx2(OriginalItem,List,ReplacedItem,List2,IndexItemReplace,0).
substituteIdx2(_,[],_,[],_,_):-!.
substituteIdx2(OriginalItem,[H|T],ReplacedItem,[ReplacedItem|T2],Idx,Idx):-
  H =:= OriginalItem,
  Idx2 is Idx + 1,
  substituteIdx2(OriginalItem , T , ReplacedItem , T2 , Idx , Idx2),!.
substituteIdx2(OriginalItem,[H|T],ReplacedItem,[H|T2],IndexItemReplace,Idx):-
  Idx2 is Idx + 1,
  substituteIdx2(OriginalItem , T , ReplacedItem , T2 , IndexItemReplace , Idx2).

substitute(_,[],_,[]):-!.
substitute(OriginalItem , [H|T] , ReplacedItem , [H|T2]):-
  OriginalItem \= H,
  substitute(OriginalItem , T , ReplacedItem , T2),!.
substitute(OriginalItem , [_|T] , ReplacedItem , [ReplacedItem|T2]):-
  substitute(OriginalItem , T , ReplacedItem , T2).

nth(Idx,List,N):-
  nth2(Idx,0,List,N).
nth2(N,N,[H|_],H):-!.
nth2(Idx,Idx2,[_|T],N):-
  NewIdx2 is Idx2 + 1,
  nth2(Idx,NewIdx2,T,N).

countList([],0).
countList([_|T],N):-
  countList(T,N2),
  N is N2 + 1.

validCell(R):-
  R =:= 0,!.

validCell(R):-
  R =:= 2.

blank_up(List,S):-
  nth(N,List,9),
  Z is N-10,
  nth(Z,List,R),
  R =:= 0,
  substituteIdx(R,List,10,Q,Z),
  substituteIdx(9,Q,R,V,N),
  substituteIdx(10,V,9,S,Z),!.

blank_up(List,S):-
  nth(N,List,9),
  Z is N-10,
  nth(Z,List,R),
  R =:= 2,
  substituteIdx(R,List,10,Q,Z),
  substituteIdx(9,Q,0,V,N),
  substituteIdx(10,V,3,S,Z),!.

blank_up(List,S):-
  nth(N,List,3),
  Z is N-10,
  nth(Z,List,R),
  R =:= 0,
  substituteIdx(R,List,10,Q,Z),
  substituteIdx(3,Q,2,V,N),
  substituteIdx(10,V,9,S,Z),!.

blank_up(List,S):-
  nth(N,List,3),
  Z is N-10,
  nth(Z,List,R),
  R =:= 2,
  substituteIdx(R,List,10,Q,Z),
  substituteIdx(3,Q,2,V,N),
  substituteIdx(10,V,3,S,Z).

checkUp(List):-
  checkUp2(List,0).

checkUp2([H|_],Idx):-
  Idx =:= 9,
  H \= 9,
  H \= 3.

checkUp2([H|T],Idx):-
  Idx < 9,
  H \= 9,
  H \= 3,
  Idx2 is Idx + 1,
  checkUp2(T,Idx2),!.

up(List,Snew):-
  checkUp(List),
  blank_up(List,Snew).

blank_down(List,S):-
  nth(N,List,9),
  Z is N+10,
  nth(Z,List,R),
  R =:= 0,
  substituteIdx(R,List,10,Q,Z),
  substituteIdx(9,Q,R,V,N),
  substituteIdx(10,V,9,S,Z),!.

blank_down(List,S):-
  nth(N,List,9),
  Z is N+10,
  nth(Z,List,R),
  R =:= 2,
  substituteIdx(R,List,10,Q,Z),
  substituteIdx(9,Q,0,V,N),
  substituteIdx(10,V,3,S,Z),!.

blank_down(List,S):-
  nth(N,List,3),
  Z is N+10,
  nth(Z,List,R),
  R =:= 0,
  substituteIdx(R,List,10,Q,Z),
  substituteIdx(3,Q,2,V,N),
  substituteIdx(10,V,9,S,Z),!.

blank_down(List,S):-
  nth(N,List,3),
  Z is N+10,
  nth(Z,List,R),
  R =:= 2,
  substituteIdx(R,List,10,Q,Z),
  substituteIdx(3,Q,2,V,N),
  substituteIdx(10,V,3,S,Z).

checkDown(List):-
  checkDown2(List,0).

checkDown2([H|_],Idx):-
  Idx =:= 89,
  H \= 9,
  H \= 3.

checkDown2([H|T],Idx):-
  Idx > 79,
  H \= 9,
  H \= 3,
  Idx2 is Idx + 1,
  checkDown2(T,Idx2),!.

checkDown2([_|T],Idx):-
  Idx =< 79,
  Idx2 is Idx + 1,
  checkDown2(T,Idx2),!.

down(List,Snew):-
  checkDown(List),
  blank_down(List,Snew).

blank_left(List,S):-
  nth(N,List,9),
  Z is N-1,
  nth(Z,List,R),
  R =:= 0,
  substituteIdx(R,List,10,Q,Z),
  substituteIdx(9,Q,R,V,N),
  substituteIdx(10,V,9,S,Z),!.

blank_left(List,S):-
  nth(N,List,9),
  Z is N-1,
  nth(Z,List,R),
  R =:= 2,
  substituteIdx(R,List,10,Q,Z),
  substituteIdx(9,Q,0,V,N),
  substituteIdx(10,V,3,S,Z),!.

blank_left(List,S):-
  nth(N,List,3),
  Z is N-1,
  nth(Z,List,R),
  R =:= 0,
  substituteIdx(R,List,10,Q,Z),
  substituteIdx(3,Q,2,V,N),
  substituteIdx(10,V,9,S,Z),!.

blank_left(List,S):-
  nth(N,List,3),
  Z is N-1,
  nth(Z,List,R),
  R =:= 2,
  substituteIdx(R,List,10,Q,Z),
  substituteIdx(3,Q,2,V,N),
  substituteIdx(10,V,3,S,Z).

checkLeft(List):-
  checkLeft2(List,0).

checkLeft2([H|_],Idx):-
  Idx =:= 80,
  H \= 9,
  H \= 3.

checkLeft2([H|T],Idx):-
  0 is Idx mod 10,
  H \= 9,
  H \= 3,
  Idx2 is Idx + 1,
  checkLeft2(T,Idx2),!.

checkLeft2([_|T],Idx):-
  Idx mod 10 =\= 0,
  Idx2 is Idx + 1,
  checkLeft2(T,Idx2),!.

left(List,Snew):-
  checkLeft(List),
  blank_left(List,Snew).

blank_right(List,S):-
  nth(N,List,9),
  Z is N+1,
  nth(Z,List,R),
  R =:= 0,
  substituteIdx(R,List,10,Q,Z),
  substituteIdx(9,Q,R,V,N),
  substituteIdx(10,V,9,S,Z),!.

blank_right(List,S):-
  nth(N,List,9),
  Z is N+1,
  nth(Z,List,R),
  R =:= 2,
  substituteIdx(R,List,10,Q,Z),
  substituteIdx(9,Q,0,V,N),
  substituteIdx(10,V,3,S,Z),!.

blank_right(List,S):-
  nth(N,List,3),
  Z is N+1,
  nth(Z,List,R),
  R =:= 0,
  substituteIdx(R,List,10,Q,Z),
  substituteIdx(3,Q,2,V,N),
  substituteIdx(10,V,9,S,Z),!.

blank_right(List,S):-
  nth(N,List,3),
  Z is N+1,
  nth(Z,List,R),
  R =:= 2,
  substituteIdx(R,List,10,Q,Z),
  substituteIdx(3,Q,2,V,N),
  substituteIdx(10,V,3,S,Z).

checkRight(List):-
  checkRight2(List,0).

checkRight2([H|_],Idx):-
  Idx =:= 89,
  H \= 9,
  H \= 3.

checkRight2([H|T],Idx):-
  N is Idx - 9,
  0 is N mod 10,
  H \= 9,
  H \= 3,
  Idx2 is Idx + 1,
  checkRight2(T,Idx2),!.

checkRight2([_|T],Idx):-
  N is Idx - 9,
  N mod 10 =\= 0,
  Idx2 is Idx + 1,
  checkRight2(T,Idx2),!.

right(List,Snew):-
  checkRight(List),
  blank_right(List,Snew).

move(S,Snew):-
    up(S,Snew).
move(S,Snew):-
    down(S,Snew).
move(S,Snew):-
    left(S,Snew).
move(S,Snew):-
    right(S,Snew).

/*
substitute(_, [], _, []):-!.
substitute(X, [X|T], Y, [Y|T1]):-
	substitute(X, T, Y, T1),!.
substitute(X, [Y|T], Y, [X|T1]):-
	substitute(X, T, Y, T1),!.
substitute(X, [H|T], Y, [H|T1]):-
	substitute(X, T, Y, T1).
*/
% end of specific part

getDiamonds(List,X,Y,T):-
  Idx is ( (X - 1) * 10 ) + Y - 1,
  nth(Idx,List,N),
  N =:= 3,
  T is 1,!.

getDiamonds(_,_,_,0).

getDistance(State,Goal,Distance):-
  getCoordinates(State,X1,Y1),
  getCoordinates(Goal,X2,Y2),
  getDistance2(X1,Y1,X2,Y2,Distance).

getDistance2(X1, Y1, X2, Y2, RES):-
  DiffX is abs(X2 - X1),
  DiffY is abs(Y2 - Y1),
  RES is DiffX + DiffY.

getDistance1(X1, Y1, X2, Y2, RES):-
  DiffX is X2 - X1,
  DiffY is Y2 - Y1,
  RES is sqrt( DiffX*DiffX + DiffY*DiffY ).

getCoordinates(List,X,Y):-
  nth(Idx,List,9),
  IdxX is Idx + 1,
  X is ceil(IdxX / 10),
  Y is IdxX mod 10,
  \+ Y =:= 0,!.

getCoordinates(List,X,Y):-
  nth(Idx,List,9),
  IdxX is Idx + 1,
  X is ceil(IdxX / 10),
  Y is 10,!.

getCoordinates(List,X,Y):-
  nth(Idx,List,3),
  IdxX is Idx + 1,
  X is ceil(IdxX / 10),
  Y is IdxX mod 10,
  \+ Y =:= 0,!.

getCoordinates(List,X,Y):-
  nth(Idx,List,3),
  IdxX is Idx + 1,
  X is ceil(IdxX / 10),
  Y is 10.

getSolution(Start , Goal , Grid):-
  go(Start,Goal,ReverseSolution,Nodes),
  lengthList(Nodes,Length),
  format('~s ~w ~n' , ["Number Of Nodes : " , Length]),
  reverse(ReverseSolution,Solution),
  getSolutionGrid(Start,Solution,Grid,0).

getSolutionGrid(X,[],X,_).
getSolutionGrid(Start,[H|T],Grid,Idx):-
  Idx2 is Idx + H,
  nth(0,Start,N),
  substituteIdx(0,Start,N,NewStart,Idx2),
  getSolutionGrid(NewStart,T,Grid,Idx2).

go(Start,Goal,ReverseSolution,Nodes):-
  getHeuristic(Start, H,D, Goal),
  path([[Start,null, 0, H, H,D]],[],Goal,ReverseSolution,Nodes).

path([], _, _ ,[],_):-
		write('No solution'),nl,!.

path(Open, Closed, Goal,ReverseSolution,Closed):-
		getBestChild(Open, [Goal, Parent, PC, H, TC , D]),%, RestOfOpen),
		write('A solution is found'),  nl ,
    write('Number Of diamonds = '),write(D),nl,
		printsolution([Goal,Parent, PC, H, TC, D], Closed,ReverseSolution),!.

path(Open, Closed, Goal,ReverseSolution,Nodes):-
    getBestChild(Open, [State, Parent, PC, H, TC, D]),%, RestOfOpen),
		write('Best child chosen is '),format('~w ~s ~w ~s ~w ~s ~w ~s ~w ~n',[State,'withPC',PC,'withH',H,'withD',D, 'with TC', TC]),
    getchildren(State, Open, Closed, Children, PC, Goal),
		addListToOpen(Children , RestOfOpen, NewOpen),
    removeFromList(State, NewOpen, RestOpen),
    path(RestOpen, [[State, Parent, PC, H, TC , D] | Closed], Goal,ReverseSolution,Nodes).

getchildren(State, Open ,Closed , Children, PC, Goal):-
		bagof(X, moves( State, Open, Closed, X, PC, Goal), Children) , !.
getchildren(_,_,_, [],_,_).

addListToOpen(Children,[],Children).
addListToOpen(L,[H|Open],[H|NewOpen]):-
      addListToOpen(L,Open,NewOpen).

removeFromList(_, [], []):-!.
removeFromList(H, [H|T], V):-
	!, removeFromList(H, T, V).
removeFromList(H, [H1|T], [H1|T1]):-
	removeFromList(H, T, T1).

removeFromOpen([State|RestOpen], State, RestOpen).

getBestChild([Child], Child).%, []).
getBestChild(Open, Best):-%, RestOpen):-
	getBestChild1(Open, Best).

getBestChild1([State], State):-!.
getBestChild1([State|Rest], Best):-
	getBestChild1(Rest, Temp),
	getBest(State, Temp, Best).

getBest([State, Parent, PC, H, TC, D], [_, _, _, _, TC1,D1], [State, Parent, PC, H, TC,D]):-
	TC > TC1, !.

getBest([State, Parent, PC, H, TC, D], [_, _, _, _, TC1,D1], [State, Parent, PC, H, TC,D]):-
	TC =:= TC1,
  D > D1, !.

getBest([_, _, _, _, TC,D], [State1, Parent1, PC1, H1, TC1,D1], [State1, Parent1, PC1, H1, TC1,D1]).
  TC =:= TC1,
  D1 => D,!.

getBest([_, _, _, _, _,_], [State1, Parent1, PC1, H1, TC1,D1], [State1, Parent1, PC1, H1, TC1,D1]).

moves( State, Open, Closed,[Next,State, NPC, H, TC,D], PC, Goal):-
		move(State,Next),
		\+ member([Next, _, _, _, _],Open),
		\+ member([Next, _, _, _, _],Closed),
		NPC is PC + 1,
    getHeuristic(Next, H,D, Goal),
		TC is NPC + H.

getHeuristic(Start, H , D, Goal):-
  getDistance(Start,Goal,Dis),
  getCoordinates(Start,X,Y),
  getDiamonds(Start,X,Y,D),
  H is Dis + D.

printsolution([State, null, PC, H, TC,D],_,_):-!,
		write(State), write(' PC: '), write(PC), write(' H:'), write(H), write(' TC: '), write(TC), nl.
printsolution([State, Parent, PC, H, TC,D], Closed,[N3|T]):-
		member([Parent, GrandParent, PC1, H1, TC1,D1], Closed),
		printsolution([Parent, GrandParent, PC1, H1, TC1,D1], Closed,T),!,
		write(State), write(' PC: '), write(PC), write(' H:'), write(H), write(' TC: '), write(TC), nl,
    getDirection(State,Parent,N3),nl.

getDirection(State,Parent,NN):-
  nth(N,State,9),
  nth(N2,Parent,9),
  NN is N - N2,
  writeDirection(NN).

writeDirection(N):-
  N =:= -10,
  write(' <Up>').

writeDirection(N):-
  N =:= 10,
  write(' <Down>').

writeDirection(N):-
  N =:= -1,
  write(' <Left>').

writeDirection(N):-
  N =:= 1,
  write(' <Right>').
