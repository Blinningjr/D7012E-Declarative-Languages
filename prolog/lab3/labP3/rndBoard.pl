% rndBoard
%
% The code below generates random boards. It is possible to (indirectly) set the expected
% percentage of contents in squares by changing the list that random_member is called
% upon.
%
% The names of procedures (rndBoardXYZ and changeXYZ) have been choosen to (hopefully)
% not coincide with other names in your code. 
%
% To use, copy the code below into your othelleo.pl and change the line
%
% initialize(InitialState, 1) :- initBoard(InitialState).
%
% to
%
% initialize(InitialState, 1) :- rndBoard(InitialState).
%
% /Håkan Jonsson, LTU

emptyBoardXYZ([[.,.,.,.,.,.], 
               [.,.,.,.,.,.],  
	       [.,.,.,.,.,.], 
	       [.,.,.,.,.,.], 
               [.,.,.,.,.,.], 
	       [.,.,.,.,.,.] ]).

rndBoardXYZ(NewB) :-
  emptyBoardXYZ(B),
  X=5, Y=5, % we fill out the board "backwards" from [5,5] to [0,0]
  changeXYZ(B,X,Y,NewB). %, showState(NewB). 

changeXYZ(B,_,Y,B) :- % done when Y<0
  Y<0,!.
changeXYZ(B,X,Y,NewB) :- % change row to Y-1 when X<0, and set X back to 5 again
  X<0, Y2 is Y-1, X2 is 5,
changeXYZ(B,X2,Y2,NewB).
changeXYZ(B,X,Y,NewB) :- % place random content on square [X,Y], and recur
  random_member(V,[1,2,'.']), % Equal probability
  % random_member(V,[1,2,'.','.']), % 50% '.', 25% 1, and 25% 2
  % random_member(V,[1,1,2,2,'.']), % 20% '.', 40% 1, and 40% 2
  set(B,B2,[X,Y],V),
  X2 is X-1,
  changeXYZ(B2,X2,Y,NewB).
