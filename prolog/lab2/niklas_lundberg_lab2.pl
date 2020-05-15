%-- Niklas Lundberg

%---------------------------------------------------------------------------------------------------
%-- Functions for calculating all the sublists of a list -------------------------------------------
%---------------------------------------------------------------------------------------------------

% Concatinating.
concat([], YS, YS).
concat([X|XS], YS, [X|Rest]) :- concat(XS, YS, Rest).


% size of list.
% sizeOf :: [a] -> Int
sizeOf([], 0).
sizeOf([_|XS], Size1) :- sizeOf(XS, Size2),
	Size1 is Size2 + 1.


%-- Removes the last element in a list
%rmLast :: [Int] -> [Int]
rmLast([], []).
rmLast([_], []).
rmLast([X|XS], [X|YS]) :- rmLast(XS, YS).


%-- Sums all elements in a list
%sumArr :: [Int] -> Int
sumArr([], 0).
sumArr([X|XS], Sum) :- sumArr(XS, RestSum), Sum is RestSum + X.


% Calculate all J sublists.
% allSubJ :: (Int, Int) -> [Int] -> [(Int, (Int, Int), [Int])]
allSubJ((_,_), [], []).
allSubJ((I, J), XS, [Y|YS]) :- sumArr(XS, Sum),
	Y = (Sum, (I,J), XS),
       	NJ is J - 1,
	rmLast(XS, NXS),
	allSubJ((I, NJ), NXS, YS).


% Calculate all I sublists.
% allSubI :: (Int, Int) -> [Int] -> [(Int, (Int, Int), [Int])]
allSubI((I, J), [X], [(X, (I, J), [X])]).
allSubI((I, J), [X|XS], YS) :- NI is I + 1,
	allSubJ((I, J), [X|XS], AllJ),
	allSubI((NI,J), XS, AllI),
	concat(AllJ, AllI, YS).


%-- Calculates all possible sublists in form [(Size, (i, j), [SubList])]
%allSubArr :: [Int] -> [(Int, (Int, Int), [Int])]
allSubArr([], []).
allSubArr(XS, YS) :- sizeOf(XS, Size),
	allSubI((1, Size), XS, YS).
 

%---------------------------------------------------------------------------------------------------
%---------------------------------------------------------------------------------------------------

%---------------------------------------------------------------------------------------------------
%-- Functions for getting the k smallest set of all the sublists -----------------------------------
%---------------------------------------------------------------------------------------------------

% Helper functions for qicksort.
lessEq((X, (_,_), _), (Y, (_,_), _)) :- X =< Y.

greaterThen((X, (_,_), _), (Y, (_,_), _)) :- X > Y.

pivot(_, [], [], []).
pivot(P, [X|XS], [X|LEQ], GT) :- lessEq(X, P),
	pivot(P, XS, LEQ, GT).
pivot(P, [X|XS], LEQ, [X|GT]) :- greaterThen(X, P),
	pivot(P, XS, LEQ, GT).

%-- Sorts a list of all sublists on the size 
%quicksort :: [(Int, (Int, Int), [Int])] -> [(Int, (Int, Int), [Int])]
quicksort([], []).
quicksort([X|XS], YS) :- pivot(X, XS, LEQ, GT),
	quicksort(LEQ, LHS),
	quicksort(GT, RHS),
	concat(LHS, [X|RHS], YS).


%-- Takes the first k elements from a list
%takeFirstK :: Int -> [a] -> [a]
takeFirstK(_, [], []).
takeFirstK(K, [X|XS], YS) :- K > 0, 
	NK is K - 1,
	takeFirstK(NK, XS, Rest),
	YS = [X|Rest];
	YS = [].


%---------------------------------------------------------------------------------------------------
%---------------------------------------------------------------------------------------------------

%---------------------------------------------------------------------------------------------------
%-- Functions for printing the reslut --------------------------------------------------------------
%---------------------------------------------------------------------------------------------------

% Concat a list of string.
listConcat([], '').
listConcat([X|XS], ST) :- listConcat(XS, ST1),
	string_concat(X, ST1, ST).

%-- Pad with space in front of string to make it length n
%padLeft :: Int -> String -> String
padLeft(N, ST, NST) :- string_length(ST, Len),
	Len >= N,
	NST = ST;
	string_concat(' ', ST, ST1),
	padLeft(N, ST1, NST).


%-- Turn list of Int:s into a string
%myShowList :: [Int] -> String
myShowList([], '[]').
myShowList([X|XS], ST) :- string_concat('[', X, ST1),
	helpShowList(XS, ST2),
	string_concat(ST1, ST2, ST).

%  hmyShowList :: [Int] -> String
helpShowList([], ']').
helpShowList([X|XS], ST) :- string_concat(',', X, ST1),
	helpShowList(XS, ST2),
	string_concat(ST1, ST2, ST).


%-- Creates a line string of the sublist data 
%strLine :: Int -> (Int, (Int, Int), [Int]) -> String
strLine(N, (Size, (I, J), SubList), ST) :- Pad = '  ',
	padLeft(N, Size, SizeS),
	padLeft(N, I, IS),
	padLeft(N, J, JS),
	myShowList(SubList, SubListS),
	listConcat([SizeS, Pad, IS, Pad, JS, Pad, SubListS], ST).


%-- Creates the result string for the k smallest sublist data
%strLines :: Int -> [(Int, (Int, Int), [Int])] -> String
strLines(PadSize, XS, ST) :- Pad = '  ',
	padLeft(PadSize, 'size', SizeS),
	padLeft(PadSize, 'i', IS),
	padLeft(PadSize, 'j', JS),
	padLeft(PadSize, 'sublist\n', SubS),
	helpStrLines(PadSize, XS, Lines),
	listConcat([SizeS, Pad, IS, Pad, JS, Pad, SubS, Lines], ST).

helpStrLines(_, [], '').
helpStrLines(PadSize, [X|XS], ST) :- strLine(PadSize, X, Y),
	helpStrLines(PadSize, XS, YS),
	listConcat([Y,'\n', YS], ST).
	

%---------------------------------------------------------------------------------------------------
%---------------------------------------------------------------------------------------------------

%---------------------------------------------------------------------------------------------------
%-- Final functions which computes the k smallest set and turns it into a String -------------------
%--------------------------------------------------------------------------------------------------- 

%smallestKset :: [Int] -> Int -> IO ()
smallestKset([], _, _) :- fail.
smallestKset(XS, K) :- PadSize = 4,
	myShowList(XS, Input),
	allSubArr(XS, SubArrs),
	quicksort(SubArrs, SortSubArrs),
	takeFirstK(K, SortSubArrs, FirstK),
	strLines(PadSize, FirstK, Lines),
	listConcat(['\nEntire list: ', Input, '\n\n', Lines], ST),
	write(ST),
	!.

%---------------------------------------------------------------------------------------------------
%---------------------------------------------------------------------------------------------------
% test1 [x*(-1)^x | x <- [1..100]]

test1(101, []).
test1(X, [Y|YS]) :- Y is X *(-1)^X,
	XP1 is X + 1,
	test1(XP1, YS).

