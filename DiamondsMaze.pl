
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

blank_up(List,S):-
  nth(N,List,9),
  Z is N-10,
  nth(Z,List,R),
  R =:= 0,
  substituteIdx(R,List,10,Q,Z),
  substituteIdx(9,Q,R,V,N),
  substituteIdx(10,V,9,S,Z).

checkUp(List):-
  checkUp2(List,0).

checkUp2([H|_],Idx):-
  Idx =:= 9,
  H \= 9.

checkUp2([H|T],Idx):-
  Idx < 9,
  H \= 9,
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
  substituteIdx(10,V,9,S,Z).

checkDown(List):-
  checkDown2(List,0).

checkDown2([H|_],Idx):-
  Idx =:= 89,
  H \= 9.

checkDown2([H|T],Idx):-
  Idx > 79,
  H \= 9,
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
  substituteIdx(10,V,9,S,Z).

checkLeft(List):-
  checkLeft2(List,0).

checkLeft2([H|_],Idx):-
  Idx =:= 80,
  H \= 9.

checkLeft2([H|T],Idx):-
  0 is Idx mod 10,
  H \= 9,
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
  substituteIdx(10,V,9,S,Z).

checkRight(List):-
  checkRight2(List,0).

checkRight2([H|_],Idx):-
  Idx =:= 89,
  H \= 9.

checkRight2([H|T],Idx):-
  N is Idx - 9,
  0 is N mod 10,
  H \= 9,
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

getSolution(Start , Goal , Grid):-
  go(Start,Goal,ReverseSolution,Nodes),
  lengthList(Nodes,Length),
  format('~s ~w ~n' , ["Number Of Nodes : " , Length]),
  reverse(ReverseSolution,Solution),
  getSolutionGrid(Start,Solution,Grid,0),!.

getSolutionGrid(X,[],X,_).
getSolutionGrid(Start,[H|T],Grid,Idx):-
  Idx2 is Idx + H,
  nth(0,Start,N),
  substituteIdx(0,Start,N,NewStart,Idx2),
  getSolutionGrid(NewStart,T,Grid,Idx2).

go(Start,Goal,ReverseSolution,Nodes):-
  path([[Start,null]],[],Goal,ReverseSolution,Nodes).

path([],_,_,[],_):-
      write('No solution'),nl,!.

path([[Goal,Parent] | _], Closed, Goal,ReverseSolution,Closed):-
      write('A solution is found'), nl ,
      printsolution([Goal,Parent],Closed,ReverseSolution).

path(Open, Closed, Goal,ReverseSolution,Nodes):-
      removeFromOpen(Open, [State, Parent], RestOfOpen),
      getchildren(State, Open, Closed, Children),
      addListToOpen(Children , RestOfOpen, NewOpen),
      path(NewOpen, [[State, Parent] | Closed], Goal,ReverseSolution,Nodes).

getchildren(State, Open ,Closed , Children):-
      bagof(X, moves( State, Open, Closed, X), Children), ! .

getchildren(_,_,_,[]).

addListToOpen(Children,[],Children).

addListToOpen(L,[H|Open],[H|NewOpen]):-
      addListToOpen(L,Open,NewOpen).

removeFromOpen([State|RestOpen], State, RestOpen).

moves(State, Open, Closed,[Next,State]):-
      move(State,Next),
      \+ member([Next,_],Open),
      \+ member([Next,_],Closed).

printsolution([_, null],_,[]).
printsolution(/*[State, null]*/[_,null],_,_):-
      /*write(State),*/write(' <Start>'),nl,!.

printsolution([State, Parent], Closed,[N3|T]):-
      member([Parent, GrandParent], Closed),
      printsolution([Parent, GrandParent], Closed,T),!,
      %write(State),
      getDirection(State,Parent,N3), nl.

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
