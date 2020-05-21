/* ------------------------------------------------------- */
%
%    D7012E Declarative languages
%    Luleå University of Technology
%
%    Student full name: Niklas Lundberg 
%    Student user id  : inaule-6 
%
/* ------------------------------------------------------- */



%do not chagne the follwoing line!
%:- ensure_loaded('play.pl').
:- ensure_loaded('stupid.pl').


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% /* ------------------------------------------------------ */
%               IMPORTANT! PLEASE READ THIS SUMMARY:
%       This files gives you some useful helpers (set &get).
%       Your job is to implement several predicates using
%       these helpers. Feel free to add your own helpers if
%       needed, as long as you write comments (documentation)
%       for all of them. 
%
%       Implement the following predicates at their designated
%       space in this file. You might like to have a look at
%       the file  ttt.pl  to see how the implementations is
%       done for game tic-tac-toe.
%
%          * initialize(InitialState,InitialPlyr). 		Done
%          * winner(State,Plyr)                                 DOne
%          * tie(State)                                         Done
%          * terminal(State)					Done
%          * moves(Plyr,State,MvList)				Done
%          * nextState(Plyr,Move,State,NewState,NextPlyr)       Done
%          * validmove(Plyr,State,Proposed)                     Done
%          * h(State,Val)  (see question 2 in the handout)	
%          * lowerBound(B)					Done
%          * upperBound(B)					Done
% /* ------------------------------------------------------ */







% /* ------------------------------------------------------ */

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% We use the following State Representation: 
% [Row0, Row1 ... Rown] (ours is 6x6 so n = 5 ).
% each Rowi is a LIST of 6 elements '.' or '1' or '2' as follows: 
%    . means the position is  empty
%    1 means player one has a stone in this position
%    2 means player two has a stone in this position. 





% DO NOT CHANGE THE COMMENT BELOW.
%
% given helper: Inital state of the board

initBoard([ [.,.,.,.,.,.], 
            [.,.,.,.,.,.],
	    [.,.,1,2,.,.], 
	    [.,.,2,1,.,.], 
            [.,.,.,.,.,.], 
	    [.,.,.,.,.,.] ]).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%% IMPLEMENT: initialize(...)%%%%%%%%%%%%%%%%%%%%%
%%% Using initBoard define initialize(InitialState,InitialPlyr). 
%%%  holds iff InitialState is the initial state and 
%%%  InitialPlyr is the player who moves first. 


% * initialize(InitialState,InitialPlyr)  
%   - returns an initial game state and Initial player 
%     (for the initial game state  you can use initBoard(B))


initialize(InitialState, 1) :- initBoard(InitialState).


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%winner(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define winner(State,Plyr) here.  
%     - returns winning player if State is a terminal position and
%     Plyr has a higher score than the other player 


score([], 0, 0) :-
	!.

score([.|XS], SP1, SP2) :-
	score(XS, SP1, SP2),
	!.

score([1|XS], NSP1, SP2) :-
	score(XS, SP1, SP2),
	NSP1 is SP1 + 1,
	!.

score([2|XS], SP1, NSP2) :-
	score(XS, SP1, SP2),
	NSP2 is SP2 + 1,
	!.

score([X|XS], SP1, SP2) :-
	score(X, SP1X, SP2X),
	score(XS, SP1XS, SP2XS),
	SP1 is SP1X + SP1XS,
	SP2 is SP2X + SP2XS,
	!.


winner(State, 1) :-
	terminal(State), 
	score(State, SP1, SP2),
	!,
	SP1 < SP2,
	!.

winner(State, 2) :-
	terminal(State), 
	score(State, SP1, SP2),
	!,
	SP1 > SP2,
	!.


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%tie(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define tie(State) here. 
%    - true if terminal State is a "tie" (no winner) 


tie(State) :-
	terminal(State),
	!,
	score(State, SP1, SP2),
	!,
	SP1 =:= SP2,
	!.


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%terminal(...)%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% define terminal(State). 
%   - true if State is a terminal   


terminal(State) :- 
	moves(1, State, MvP1),
	moves(2, State, MvP2),
	length(MvP1, Len1),
	length(MvP2, Len2),
	!,
	0 =:= Len1 + Len2,
	!.


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%showState(State)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% given helper. DO NOT  change this. It's used by play.pl
%%

showState( G ) :- 
	write('  0 1 2 3 4 5'),
	nl,
	printRows( G, 0). 
 
printRows( [], _ ). 
printRows( [H|L], N ) :-
       	write(N),	
	write(' '),
	printList(H),
	nl,
	NP1 is N + 1,
	printRows(L, NP1). 

printList([]).
printList([H | L]) :-
	write(H),
	write(' '),
	printList(L).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%moves(Plyr,State,MvList)%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define moves(Plyr,State,MvList). 
%   - returns list MvList of all legal moves Plyr can make in State
%


validMoves(Plyr, State, [5, 5], MvList) :- 
	validmove(Plyr, State, [5, 5]),
	!,
	MvList = [[5, 5]];
	MvList = [],
	!.

validMoves(Plyr, State, [X, 5], MvList) :- 
	validmove(Plyr, State, [X, 5]),
	!,
	X1 is X + 1,
	validMoves(Plyr, State, [X1, 0], Rest),
	MvList = [[X, 5]|Rest],
	!;

	X1 is X + 1,
	validMoves(Plyr, State, [X1, 0], MvList),
	!.

validMoves(Plyr, State, [X, Y], MvList) :-
	validmove(Plyr, State, [X, Y]),
	!,
	Y1 is Y + 1,
	validMoves(Plyr, State, [X, Y1], Rest),
	MvList = [[X, Y]|Rest],
	!;
	
	Y1 is Y + 1,
	validMoves(Plyr, State, [X, Y1], MvList),
	!.


moves(Plyr, State, MvList) :-
	validMoves(Plyr, State, [0, 0], MvList),
	!.


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%nextState(Plyr,Move,State,NewState,NextPlyr)%%%%%%%%%%%%%%%%%%%%
%% 
%% define nextState(Plyr,Move,State,NewState,NextPlyr). 
%   - given that Plyr makes Move in State, it determines NewState (i.e. the next 
%     state) and NextPlayer (i.e. the next player who will move).
%


otherPlyr(1, 2).
otherPlyr(2, 1).


flipDir(Plyr, _,  State, Pos, _, State) :-
	get(State, Pos, Plyr),
	!.

flipDir(Plyr, OState, State, [X, Y], [DX, DY], NewState) :- 
	otherPlyr(Plyr, OPlyr),
	get(State, [X, Y], OPlyr),
	!,
	set(State, State1, [X, Y], Plyr), 
	!,
	X1 is X + DX,
	Y1 is Y + DY,
	flipDir(Plyr, OState, State1, [X1, Y1], [DX, DY], NewState),
	!.

flipDir(_, OState, _, _, _, OState).


flip(Plyr, State1, [X, Y], State9) :- 
	XP is X + 1, XM is X - 1,
	YP is Y + 1, YM is Y - 1,
	flipDir(Plyr, State1, State1, [XP, Y], [1, 0], State2),
	!,
	flipDir(Plyr, State2, State2, [XM, Y], [-1, 0], State3),
	!,
	flipDir(Plyr, State3, State3, [X, YP], [0, 1], State4),
	!,
	flipDir(Plyr, State4, State4, [X, YM], [0, -1], State5),
	!,
	flipDir(Plyr, State5, State5, [XP, YP], [1, 1], State6),
	!,
	flipDir(Plyr, State6, State6, [XP, YM], [1, -1], State7),
	!,
	flipDir(Plyr, State7, State7, [XM, YM], [-1, -1], State8),
	!,
	flipDir(Plyr, State8, State8, [XM, YP], [-1, 1], State9),
	!.


nextState(Plyr, 'n', State, State, NextPlyr) :-
	validmove(Plyr, State, 'n'),
	!,
	otherPlyr(Plyr, NextPlyr),
	!.

nextState(Plyr, Move, State1, State3, NextPlyr) :-
	otherPlyr(Plyr, NextPlyr),
	set(State1, State2, Move, Plyr),
	!,
	flip(Plyr, State2, Move, State3),
	!.


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%validmove(Plyr,State,Proposed)%%%%%%%%%%%%%%%%%%%%
%% 
%% define validmove(Plyr,State,Proposed). 
%   - true if Proposed move by Plyr is valid at State.
  

checkDir(Plyr, State, Pos, _) :-
	get(State, Pos, Plyr),
	!.

checkDir(Plyr, State, [X, Y], [DX, DY]) :-
	otherPlyr(Plyr, OPlyr),
	get(State, [X, Y], OPlyr),
	!,
	X1 is X + DX,
	Y1 is Y + DY,
	checkDir(Plyr, State, [X1, Y1], [DX, DY]),
	!.


checkFirst(Plyr, State, [X, Y], [DX, DY]) :-
	otherPlyr(Plyr, OPlyr),
	get(State, [X, Y], OPlyr),
	!,
	X1 is X + DX,
	Y1 is Y + DY,
	checkDir(Plyr, State, [X1, Y1], [DX, DY]),
	!.


dirs([X, Y], [XP, Y], [1, 0]) :- XP is X + 1.			% Right
dirs([X, Y], [XM, Y], [-1, 0]) :- XM is X - 1.			% Left
dirs([X, Y], [X, YP], [0, 1]) :- YP is Y + 1.			% Up
dirs([X, Y], [X, YM], [0, -1]) :- YM is Y - 1.			% Down
dirs([X, Y], [XP, YP], [1, 1]) :- XP is X + 1, YP is Y + 1.	% Right UP
dirs([X, Y], [XP, YM], [1, -1]) :- XP is X + 1, YM is Y - 1.	% Right Down
dirs([X, Y], [XM, YM], [-1, -1]) :- XM is X - 1, YM is Y - 1.	% Left Down
dirs([X, Y], [XM, YP], [-1, 1]) :- XM is X - 1, YP is Y + 1.	% Left Up


validmove(Plyr, State, Pos) :- 
	get(State, Pos, '.'),
	!,
	dirs(Pos, NextPos, DeltaPos),
	checkFirst(Plyr, State, NextPos, DeltaPos),
	!.

validmove(Plyr, State, 'n') :-
	moves(Plyr, State, MV),
	!,
	MV = [].


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%h(State,Val)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define h(State,Val). 
%   - given State, returns heuristic Val of that state
%   - larger values are good for Max, smaller values are good for Min
%   NOTE1. If State is terminal h should return its true value.
%   NOTE2. If State is not terminal h should be an estimate of
%          the value of state (see handout on ideas about
%          good heuristics.

h(state, 100) :-
	terminal(State), 
	winner(State, 2),
	!.

h(state, -100) :-
	terminal(State),
	winner(State, 1),
	!.

	h(state, 0) :-
	terminal(State),
	tie(State),
	!.

h(State, Val) :- 
	moves(1, State, MvL1),
	moves(2, State, MvL2),
	length(MvL1, Len1),
	length(MvL2, Len2),
	Val is Len2 * 4 - Len1 * 4,
	!.


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%lowerBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define lowerBound(B).  
%   - returns a value B that is less than the actual or heuristic value
%     of all states.


lowerBound(-101).


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%upperBound(B)%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% define upperBound(B). 
%   - returns a value B that is greater than the actual or heuristic value
%     of all states.


upperBound(101).


% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                       %
%                                                                       %
%                Given   UTILITIES                                      %
%                   do NOT change these!                                %
%                                                                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get(Board, Point, Element)
%    : get the contents of the board at position column X and row Y
% set(Board, NewBoard, [X, Y], Value):
%    : set Value at column X row Y in Board and bind resulting grid to NewBoard
%
% The origin of the board is in the upper left corner with an index of
% [0,0], the upper right hand corner has index [5,0], the lower left
% hand corner has index [0,5], the lower right hand corner has index
% [5,5] (on a 6x6 board).
%
% Example
% ?- initBoard(B), showState(B), get(B, [2,3], Value). 
%. . . . . . 
%. . . . . . 
%. . 1 2 . . 
%. . 2 1 . . 
%. . . . . . 
%. . . . . . 
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], 
%     ['.', '.', 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], 
%     ['.', '.', '.', '.'|...], ['.', '.', '.'|...]]
%Value = 2 
%Yes
%?- 
%
% Setting values on the board
% ?- initBoard(B),  showState(B),set(B, NB1, [2,4], 1),
%         set(NB1, NB2, [2,3], 1),  showState(NB2). 
%
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 2 1 . . 
% . . . . . . 
% . . . . . .
% 
% . . . . . . 
% . . . . . . 
% . . 1 2 . . 
% . . 1 1 . . 
% . . 1 . . . 
% . . . . . .
%
%B = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.', 
%1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', '.', '.'|...], ['.', '.',
% '.'|...]]
%NB1 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 2, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', '.
%', '.'|...]]
%NB2 = [['.', '.', '.', '.', '.', '.'], ['.', '.', '.', '.', '.', '.'], ['.', '.'
%, 1, 2, '.', '.'], ['.', '.', 1, 1, '.'|...], ['.', '.', 1, '.'|...], ['.', 
%'.', '.'|...]]

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% get(Board, Point, Element): get the value of the board at position
% column X and row Y (indexing starts at 0).
% Do not change get:

get( Board, [X, Y], Value) :- 
	nth0( Y, Board, ListY), 
	nth0( X, ListY, Value).

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% set( Board, NewBoard, [X, Y], Value): set the value of the board at position
% column X and row Y to Value (indexing starts at 0). Returns the new board as
% NewBoard. Do not change set:

set( [Row|RestRows], [NewRow|RestRows], [X, 0], Value) :-
    setInList(Row, NewRow, X, Value). 

set( [Row|RestRows], [Row|NewRestRows], [X, Y], Value) :-
    Y > 0, 
    Y1 is Y-1, 
    set( RestRows, NewRestRows, [X, Y1], Value). 

% DO NOT CHANGE THIS BLOCK OF COMMENTS.
%
% setInList( List, NewList, Index, Value): given helper to set. Do not
% change setInList:

setInList( [_|RestList], [Value|RestList], 0, Value). 

setInList( [Element|RestList], [Element|NewRestList], Index, Value) :- 
	Index > 0, 
	Index1 is Index-1, 
	setInList( RestList, NewRestList, Index1, Value). 
 
