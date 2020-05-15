% Niklas Lundberg
% state(Robot position, Brass key, Steel key, Package, number of item held)

% Pickup Package.
move(state(Pos, BK, SK, Pos, NI1),
	pickup(package),
	state(Pos, BK, SK, has, NI2)) :- NI1 < 2, NI2 is NI1 + 1.

% Walk from r3 to r1.
move(state(r3, has, SK, Pa, NI),
	walk(r3, r1),
	state(r1, has, SK, Pa, NI)).

% Drop Package. 
move(state(Pos, BK, SK, has, NI1),
	drop(package),
	state(Pos, BK, SK, Pos, NI2)) :- NI2 is NI1 - 1.

% Walk from r1 to r3.
move(state(r1, has, SK, Pa, NI),
	walk(r1, r3),
	state(r3, has, SK, Pa, NI)).

% Walk from r1 to r2.
move(state(r1, BK, has, Pa, NI),
	walk(r1, r2),
	state(r2, BK, has, Pa, NI)).

% Pickup Brass Key.
move(state(Pos, Pos, SK, Pa, NI1),
	pickup(brasskey),
	state(Pos, has, SK, Pa, NI2)) :- NI1 < 2, NI2 is NI1 + 1.

% Walk from r2 to r1.
move(state(r2, BK, has, Pa, NI),
	walk(r2, r1),
	state(r1, BK, has, Pa, NI)).

% Pickup Steel Key.
move(state(Pos, BK, Pos, Pa, NI1),
	pickup(steelkey),
	state(Pos, BK, has, Pa, NI2)) :- NI1 < 2, NI2 is NI1 + 1.

% Drop Brass Key. 
move(state(Pos, has, SK, Pa, NI1),
	drop(brasskey),
	state(Pos, Pos, SK, Pa, NI2)) :- NI2 is NI1 - 1.

% Drop Steel Key. 
move(state(Pos, BK, has, Pa, NI1),
	drop(steelkey),
	state(Pos, BK, Pos, Pa, NI2)) :- NI2 is NI1 - 1.


solvR(state(r2,_,_,has,_), _, []).
solvR(State1, N1, [Action|Actions]) :- N1 > 0,
	move(State1, Action, State2),
	N2 is N1 - 1,
	solvR(State2, N2, Actions).


