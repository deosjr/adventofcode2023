:- use_module(library(assoc)).
:- use_module(library(between)).
:- use_module(library(charsio)).
:- use_module(library(lambda)).

%use_test_input.

run_day(22, Filename) :-
    phrase_from_file(parse(Bricks), Filename),
    settle_bricks(Bricks, Settled),
    %write(Settled),nl,
    safe_disintegrate(Settled, Ans1),
    write_part1(Ans1).

settle_bricks(Bricks, Settled) :-
    qsort(byLowZ, Bricks, Sorted),
    empty_assoc(Assoc),
    settle_bricks(Sorted, Assoc, Settled).

settle_bricks([], _, []).
settle_bricks([Brick|T], Assoc, [NewSettled|Settled]) :-
    %write(Brick),nl,
    settle(Brick, Assoc, NewSettled),
    update_assoc(NewSettled, Assoc, Assoc0),
    settle_bricks(T, Assoc0, Settled).

settle(Brick, Assoc, NewBrick) :-
    intersect(Brick, Assoc, Z),
    Brick = (X1-Y1-Z1)/(X2-Y2-Z2),
    ZZ #= Z2 - (Z1 - Z),
    NewBrick = (X1-Y1-Z)/(X2-Y2-ZZ).

% lines through each X-Y pixel of brick downwards
rays((X1-Y-Z)/(X2-Y-Z), Rays) :-
    X1 #\= X2,
    numlist(X1, X2, Xs),
    maplist(Y+\X^C^(C=X-Y), Xs, Rays).
rays((X-Y1-Z)/(X-Y2-Z), Rays) :-
    Y1 #\= Y2,
    numlist(Y1, Y2, Ys),
    maplist(X+\Y^C^(C=X-Y), Ys, Rays).
rays((X-Y-_)/(X-Y-_), [X-Y]).

% find Z where brick settles; highest intersecting Z-value + 1 or 1 if none (which is on the floor)
% assumption: rays start higher than any potential intersecting brick (otherwise there'd be intersections in input)
intersect(Brick, Assoc, Z) :-
    rays(Brick, Rays),
    maplist(intersect(Assoc), Rays, Zs),
    sort(Zs, Sorted),
    reverse(Sorted, [Z|_]).

intersect(Assoc, Ray, Z) :-
    ( get_assoc(Ray, Assoc, ZZ) -> Z #= ZZ+1 ; Z #= 1 ). 

% B1 falling, B2 supporting
brick_intersect(B1, B2, Z) :-
    rays(B1, R1),
    rays(B2, R2),
    member(R, R1),
    member(R, R2),
    B2 = _/(_-_-ZZ),
    Z #= ZZ+1.

update_assoc(Brick, Assoc, Assoc0) :-
    Brick = _/(_-_-Z),
    rays(Brick, Rays),
    foldl(Z+\A^B^C^(put_assoc(A, B, Z, C)), Rays, Assoc, Assoc0).

safe_disintegrate(Settled, Ans) :-
    findall(Z, member(_/(_-_-Z),Settled), HighZs),
    sort(HighZs, SortedZs),
    safe_disintegrate(SortedZs, Settled, 0, Ans).
    
safe_disintegrate([], _, X, X).
safe_disintegrate([Z|ZT], Bricks, Acc, Ans) :-
    %write(Z),nl,
    ZZ #= Z+1,
    findall(B, (B=_/(_-_-Z), member(B, Bricks)), SupportingBricks),
    findall(B, (B=(_-_-ZZ)/_, member(B, Bricks)), FallingBricks),
    maplist(\X^Y^(findall(F, (member(F, FallingBricks), brick_intersect(F, X, ZZ)), Fs), Y=X-Fs), SupportingBricks, BFs),
    maplist(\X^Y^(X=B-Fs, (maplist(\Q^(member(R,SupportingBricks),R\=B,brick_intersect(Q,R,ZZ)), Fs) -> Y#=1 ; Y#= 0)), BFs, Nums),
    sum(Nums, #=, L),
    NAcc #= Acc + L,
    safe_disintegrate(ZT, Bricks, NAcc, Ans).

% From and To are already ordered such that To contains the larger coordinate value
parse([From/To|T]) --> parse_coord(From), "~", parse_coord(To), "\n", parse(T).
parse([]) --> [].

parse_coord(X-Y-Z) --> integer(X), ",", integer(Y), ",", integer(Z).

integer(N) --> digits(D), {number_chars(N, D)}.
digits([H|T]) --> [H], {char_type(H, decimal_digit)}, digits(T).
digits([X]) --> [X], {char_type(X, decimal_digit)}.

byLowZ((_-_-Z1)/_, (_-_-Z2)/_, >) :- Z1 #> Z2.
byLowZ((_-_-Z1)/_, (_-_-Z2)/_, <) :- Z1 #< Z2.
byLowZ((_-_-Z)/_, (_-_-Z)/_, =).

byHighZ(_/(_-_-Z1), _/(_-_-Z2), >) :- Z1 #< Z2.
byHighZ(_/(_-_-Z1), _/(_-_-Z2), <) :- Z1 #> Z2.
byHighZ(_/(_-_-Z), _/(_-_-Z), =).

qsort(_, [], []).
qsort(Pred, [H|U], S) :-
    pivot(Pred, H, U, L, R),
    qsort(Pred, L, SL),
    qsort(Pred, R, SR),
    append(SL, [H|SR], S).

pivot(_, _, [], [], []).
pivot(Pred, H, [U|T], [U|LS], RS) :-
    call(Pred,U,H, <), pivot(Pred, H, T, LS, RS).
pivot(Pred, H, [U|T], [U|LS], RS) :-
    call(Pred,U,H, =), pivot(Pred, H, T, LS, RS).
pivot(Pred, H, [U|T], LS, [U|RS]) :-
    call(Pred,U,H, >), pivot(Pred, H, T, LS, RS).
