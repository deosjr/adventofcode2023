:- use_module(library(between)).
:- use_module(library(charsio)).
:- use_module(library(lambda)).

%use_test_input.

run_day(22, Filename) :-
    phrase_from_file(parse(Bricks), Filename),
    settle_bricks(Bricks, Settled),
    write(Settled),
    safe_disintegrate(Settled, Ans1),
    write_part1(Ans1).

settle_bricks(Bricks, Settled) :-
    qsort(byLowZ, Bricks, Sorted),
    settle_bricks(Sorted, [], Settled).

settle_bricks([], X, X).
settle_bricks([Brick|T], Settled, Total) :-
    write(Brick),nl,
    settle(Brick, Settled, NewSettled),
    settle_bricks(T, NewSettled, Total).

settle(Brick, Settled, Out) :-
    intersect(Brick, Settled, Z),
    Brick = (X1-Y1-Z1)/(X2-Y2-Z2),
    ZZ #= Z2 - (Z1 - Z),
    NewBrick = (X1-Y1-Z)/(X2-Y2-ZZ),
    qsort(byHighZ, [NewBrick|Settled], Out).

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
intersect(Brick, Settled, Z) :-
    rays(Brick, Rays),
    intersect(Rays, Settled, Z).

intersect([], _, 1).
intersect([R|Rays], Settled, Z) :-
    ( ray_intersect(R, Settled, ZZ) -> Z #= ZZ ; intersect(Rays, Settled, Z) ).

ray_intersect(X-Y, [Brick|Settled], Z) :-
    rays(Brick, Rays),
    ( member(X-Y, Rays) -> Brick = _/(_-_-ZZ), Z#=ZZ+1 ; ray_intersect(X-Y, Settled, Z) ).

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
    findall(B, (
        select(B, SupportingBricks, Rem),
        findall(F, (
            member(F, FallingBricks),
            intersect(F, [B], ZZ)
        ), SupportedBricks),
        maplist(\X^(member(R,Rem),intersect(X,[R],ZZ)), SupportedBricks)
    ), List),
    length(List, L),
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
