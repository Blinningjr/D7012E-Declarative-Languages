%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Testboards to use when working on Lab Assignment P3 %%
%% D7012E Declarative languages, LTU                   %%
%% By Håkan Jonsson, 2018                              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% 1. Original boards (the ones that came with the assignment)
%%

% Niklas results
% Result: testBoard1(St),mmeval(2, St, Val, BestMv, 5, Sef).
% Val = 0
% BestMv = [0, 1]
% Sef = 62285
testBoard1([ [.,.,.,.,.,.], 
             [.,1,.,.,.,.],
             [.,.,2,1,.,.],
             [.,.,1,2,.,.],
             [.,.,.,.,1,.],
             [.,.,.,.,.,.] ]).

% Niklas results
% Result: testBoard2(St),mmeval(2, St, Val, BestMv, 5, Sef).
% Val = 0
% BestMv = [0, 4]
% Sef = 148642
testBoard2([ [.,2,.,.,.,2], 
             [.,.,1,.,1,.],
             [.,.,.,1,.,.],
             [.,.,1,1,1,.],
             [.,1,.,1,.,.],
             [.,.,.,2,.,.] ]).

% Niklas results
% Result: testBoard3(St),mmeval(2, St, Val, BestMv, 5, Sef).
% Val = 0
% BestMv = [0, 4]
% Sef = 84129
testBoard3([ [.,.,.,2,.,.], 
             [.,2,.,1,1,.],
             [2,1,1,1,.,.],
             [2,1,1,.,1,2],
             [.,1,.,1,.,.],
             [2,.,.,2,2,.] ]).

%%
%% 2. Testing to flip in various directions
%%

% both players can move but when player 1 moves, that move flips from right to left
% along the top row
flipRLtop([[.,1,2,2,2,.],
	   [.,.,.,.,.,.],
	   [.,.,.,.,.,.],
	   [.,.,.,.,.,.],
	   [.,.,.,.,.,.],
	   [.,.,.,.,.,.]]).

% only player 2 can move, and that move flips from left to right
% along the bottom row
flipLRbottom([[.,.,.,.,.,.],
	      [.,.,.,.,.,.],
	      [.,.,.,.,.,.],
	      [.,.,.,.,.,.],
	      [.,.,.,.,.,.],
	      [.,1,1,1,1,2]]).

% only player 2 can move, and that move flips from top to bottom
% along the left column
flipTBleft([[.,.,.,.,.,.],
            [1,.,.,.,.,.],
            [1,.,.,.,.,.],
            [1,.,.,.,.,.],
            [1,.,.,.,.,.],
            [2,.,.,.,.,.]]).

% only player 1 can move, and that move flips from bottom to top
% along the right column
flipBTright([[.,.,.,.,.,1],
	     [.,.,.,.,.,2],
	     [.,.,.,.,.,2],
	     [.,.,.,.,.,2],
	     [.,.,.,.,.,2],
	     [.,.,.,.,.,.]]).

% only player 1 can move, and that move flips along the main
% diagional from upper left to lower right
flipDiagULtoLR([[.,.,.,.,.,.],
		[.,2,.,.,.,.],
		[.,.,2,.,.,.],
		[.,.,.,2,.,.],
		[.,.,.,.,1,.],
		[.,.,.,.,.,.]]).

% only player 2 can move, and that move flips along the main
% diagional from upper right to lower left
flipDiagURtoLL([[.,.,.,.,.,.],
		[.,.,.,.,1,.],
		[.,.,.,1,.,.],
		[.,.,1,.,.,.],
		[.,1,.,.,.,.],
		[2,.,.,.,.,.]]).

% no moves possible, and no flips
noMovesNoFlipsA([[1,2,1,2,1,2],
		 [2,1,1,1,2,2],
		 [1,1,.,1,1,1],
		 [2,1,1,1,2,2],
		 [1,2,1,2,1,2],
		 [2,2,1,2,2,1]]).

% no moves possible, and no flips
noMovesNoFlipsB([[2,1,2,1,2,1],
		 [1,2,2,2,1,1],
		 [2,2,.,2,2,2],
		 [1,2,2,2,1,1],
		 [2,1,2,1,2,1],
		 [1,1,2,1,1,2]]).

% player 1 can move, and that move flips left and right only
flipLRonly1([[.,.,.,2,.,.],
	     [.,.,.,1,.,.],
	     [.,.,.,1,.,.],
	     [1,2,2,.,2,1],
	     [.,.,.,1,.,.],
	     [.,.,.,2,.,.]]).

% only player 1 can move, and that move flips in all 8 directions
flipAll8Dirs1([[1,2,1,2,1,2],
	       [2,2,2,2,2,2],
	       [1,2,.,2,2,1],
	       [2,2,2,2,2,2],
	       [1,2,2,2,2,2],
	       [2,2,1,2,2,1]]).

% only player 2 can move, and that move flips in all 8 directions
flipAll8Dirs2([[2,2,2,2,2,2],
	       [2,1,2,1,2,2],
	       [2,2,1,1,1,2],
	       [2,1,1,.,1,2],
	       [2,2,1,1,1,2],
	       [2,2,2,2,2,2]]).


%%
%% 3. Testing that TIE is detected
%%

% both players can make a move; if 1 moves first, 2 moves, and then a tie with a full board
tieInTwoMovesFullBoard([[.,2,2,1,2,2], 
			[2,2,2,2,2,1], 
			[2,2,2,2,2,1],
			[2,2,2,1,1,1], 
			[2,2,2,2,1,1],
			[1,1,1,1,1,.]]).


% an immediate tie with 4 unplayable squares
tieFourEmptyInCorners([[.,2,2,2,2,.],
		       [1,2,2,2,1,1],
		       [1,2,2,1,1,1],
		       [1,2,1,2,1,1],
		       [1,1,1,1,2,1],
		       [.,2,2,2,2,.]]).


% an immediate tie with 2 unplayable squares
tieFourEmptyOnBorders([[2,2,.,.,1,1],
		       [1,1,1,2,2,2],
		       [1,1,1,2,2,2],
		       [1,1,1,2,2,2],
		       [1,1,1,2,2,2],
		       [1,1,.,.,2,2]]).


% player 1 can make 1 move, then tie with four empty
tieFourEmptyOnly1canMove([[.,2,2,2,2,.],
			  [1,2,2,2,1,1],
			  [1,2,2,1,1,1],
			  [2,2,1,2,1,1],
			  [.,1,1,1,2,1],
			  [.,2,2,2,2,.]]).


	%Fail if start as p2.
% only player 1 can make a move, and then it's a 3-3 tie
tie30emptyOnly1canMove([[.,.,.,.,.,.],
			[.,.,.,.,.,.],
			[2,.,2,1,2,2],
			[.,.,.,.,.,.],
			[.,.,.,.,.,.],
			[.,.,.,.,.,.]]).

% only player 2 can make a move, and then it's a 3-3 tie
tie30emptyOnly2canMove([[.,.,.,.,.,.],
			[.,.,.,.,.,.],
			[1,.,1,2,1,1],
			[.,.,.,.,.,.],
			[.,.,.,.,.,.],
			[.,.,.,.,.,.]]).


%%
%% 4. Testing that WINNER is detected
%%

% both players can make one move each, and then player 1 wins
winInTwoMovesFullBoard([[.,2,1,1,1,2], 
			[2,2,2,2,2,1], 
			[1,2,2,2,2,1],
			[1,2,2,1,2,1], 
			[1,2,2,2,1,1],
			[2,1,1,1,1,.]]).

% player 1 is an immediate winner, 0-36
onlyTwos([[2,2,2,2,2,2], 
	  [2,2,2,2,2,2],
	  [2,2,2,2,2,2], 
	  [2,2,2,2,2,2], 
	  [2,2,2,2,2,2], 
	  [2,2,2,2,2,2] ]).


% player 2 is an immediate winner, 0-36
onlyOnes([[1,1,1,1,1,1], 
	  [1,1,1,1,1,1],
	  [1,1,1,1,1,1], 
	  [1,1,1,1,1,1], 
	  [1,1,1,1,1,1], 
	  [1,1,1,1,1,1] ]).


%%
%% 5. Testing null moves
%%


% Fail plyr 2 chose move: null.
% player 2 has no move, but 1 has two; then 1 wins
forcing2toDoNullMove([[.,.,.,.,.,.],
		      [.,.,.,.,.,2],
		      [.,.,.,.,.,2],
		      [.,.,.,.,.,2],
		      [.,.,.,.,.,2],
		      [.,2,2,2,2,1]]).


% player 1 has no moves, but 2 has two; then 2 wins
forcing1toDoNullMoves([[.,.,.,.,.,.],
		       [.,.,.,.,.,1],
		       [.,.,.,.,.,1],
		       [.,.,.,.,.,1],
		       [.,.,.,.,.,1],
		       [.,1,1,1,1,2]]).


