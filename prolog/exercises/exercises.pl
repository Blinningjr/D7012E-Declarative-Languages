% Figure 1.8   The family program.

parent( pam, bob).       % Pam is a parent of Bob
parent( tom, bob).
parent( tom, liz).
parent( bob, ann).
parent( bob, pat).
parent( pat, jim).

female( pam).            % Pam is female
female( liz).
female( ann).
female( pat).
male( tom).              % Tom is male
male( bob).
male( jim).

offspring( Y, X)  :-     % Y is an offspring of X if
   parent( X, Y).        % X is a parent of Y

mother( X, Y)  :-        % X is the mother of Y if
   parent( X, Y),        % X is a parent of Y and
   female( X).           % X is female

grandparent( X, Z)  :-   % X is a grandparent of Z if
   parent( X, Y),        % X is a parent of Y and
   parent( Y, Z).        % Y is a parent of Z

different(X,Y) :- \+(X=Y).

sister( X, Y)  :-        % X is a sister of Y if
   parent( Z, X),
   parent( Z, Y),        % X and Y have the same parent and
   female( X),           % X is female and
   different( X, Y).     % X and Y are different

predecessor( X, Z)  :-   % Rule prl: X is a predecessor of Z
   parent( X, Z).

predecessor( X, Z)  :-   % Rule pr2: X is a predecessor of Z
   parent( X, Y),
   predecessor( Y, Z).



length1( [], 0).
length1( [ _| Tail], N) :- length1( Tail, N1), 
                           N is 1 + N1.

length2( [], 0).
length2( [_ | Tail], N) :- length2( Tail, N1), 
                           N = 1 + N1.

length3( [], 0).
length3( [_ | Tail], N) :- N = 1 + N1, 
                           length3( Tail, N1).



% Exercise 1.1 on Page 8
% a) parent(jim,X). X = false.
% b) parent(X,jim). X = pat.
% c) parent(pam,X),parent(X,pat). X = bob.
% d) parent(pam,X),parent(X,Y),parent(Y,jim). X = bob. Y = pat.


% Exercise 1.2 on Page 8
% a) parent(X, pat).
% b) parent(liz, X).
% c) parent(X, pat),parent(Y, X).


% Exercise 1.3 on Page 14
% a) 
happy(X) :- parent(X,_).
% b) 
hastwochidren(X) :- parent(X,Y),sister(_,Y).


% Exercise 1.4 on page 14
grandchild(X,Y) :- parent(Y,Z),parent(Z,X).


% Exercise 1.6 on page 19
% yes.


% Exercise 1.7 on page 24
% a) no backtracking.
% b) no backtracking.
% c) no backtracking.
% d) backtracking.


% Exercise 2.1 on page 35
% a) Variable
% b) Atom
% c) Atom
% d) Variable
% e) Atom
% f) Structure
% g) Number
% h) error
% i) Structure
% j) Structure


% Exercise 2.2 on page 35
% point(X,Y).
% rectangel( point(X1, Y1), point(X1, Y2), point(X2, Y2), point(X2, Y1).
% square(point(-X,-Y), point(-X,Y), point(X,Y), point(X,-Y)).
% circle(point(X,Y), R).


% Exercise 2.3 on page 40
% a) A = 1, B = 2.
% b) fail
% c) fail
% d) D = 2, E = 2.
% e) P1 = point(-1,0), P2 = point(1,0), P3 = point(0, Y). Y can be any thing.


% Exercise 2.5 on page 40
vertical(point(X, _), point(X, _)).
horizontal(point(_,Y), point(_,Y)).
regular(rectangle(P1,P2,P3,P4)) :- vertical(P1,P2),
	horizontal(P2,P3),
	vertical(P3,P4),
	horizontal(P4,P1).


% Exercise 2.6 on page 42
% a) A = two
% b) false.
% c) C = one.
% d) D = s(s(1)), D =  s(s(s(s(s(1))))).


% Exercise 2.8 on page 43
translate(Number, Word) :- Number = 1, Word = one.
translate(Number, Word) :- Number = 2, Word = two.
translate(Numver, Word) :- Numver = 3, Word = three.


% Exercise 2.9 on page 48
% Done on paper.


% Exercise 3.1 on page 71
concat([], L, L).
concat([X|L1], L2, [X|L3]) :- concat(L1, L2, L3).
% a) concat(L1, [_,_,_], L).
% b) concat([_,_,_], L1, L),concat(L2, [_,_,_], L1).


% Exercise 3.2 on page 72
last1(Item, List) :- concat(_, [Item], List).
last2(Item, [Item]).
last2(Item, [_|R]) :- last2(Item, R). 


% Exercise 3.3 on page 76
evenlength([]).
evenlength([_|R]) :- oddlength(R).

oddlength([_]).
oddlength([_|R]) :- evenlength(R).


% Exercise 3.4 on page 77
reverse([], []).
reverse([F|R], RL) :- reverse(R,RL1),concat(RL1, [F], RL).


% Exercise 3.5 on page 77
palindrome(L) :- reverse(L, L).


% Exercise 3.6 on page 77
shift(L1, L2) :- concat(Rest, [Last], L1), concat([Last], Rest, L2).


% Exercise 3.7 on page 77
means(0, zero).
means(1, one).
means(2, two).
means(3, three).
means(4, four).
means(5, five).
means(6, six).
means(7, seven).
means(8, eight).
means(9, nine).

translate2([], []).
translate2([X|XS], [Y|YS]) :- means(X,Y),translate2(XS, YS).


% Exercise 3.8 on page 77
mysubset([], []).
mysubset([_|XS], YS) :- mysubset(XS, YS).
mysubset([X|XS], [X|YS]) :- mysubset(XS, YS).


% Exercise 3.10 on page 78
move(state(middle, onbox, middle, hasnot), grasp, state(middle, onbox, middle, has)).
move(state(P, onfloor, P, H), climb, state(P, onbox, P, H)).
move(state(P1, onfloor, P1, H), push(P1, P2), state(P2, onfloor, P2, H)).
move(state(P1, onfloor, B, H), walk(P1, P2), state(P2, onfloor, B, H)).

canget(state(_, _, _,has), []).
canget(State1, [Action|Actions]) :- move(State1, Action, State2), canget(State2, Actions).


% Exercise 3.11 on page 78
flatten([X|XS], Flat) :- flatten(X, Y), flatten(XS, YS),concat(Y, YS, Flat), !.
flatten([], []) :- !.
flatten(X, [X]).


% Exercise 3.16 on page 90
max(X, Y, X) :- X >= Y, !.
max(X, Y, Y) :- X < Y.


% Exercise 3.18 on page 90
sumlist([], 0).
sumlist([X|XS], Sum) :- sumlist(XS, S), Sum is X + S.


% Exercise 3.21 on page 90
between(N1, N2, X) :- N1 <= X, X <= N2.

