:- use_module(library(between)).
:- use_module(library(charsio)).
:- use_module(library(lambda)).

:- dynamic([point/1, rectangle/1, overlap/1]).

%use_test_input.

run_day(18, Filename) :-
    phrase_from_file(parse(List), Filename),
    points(0-0, List, Points),
    qsort(byY, Points, PointsByY),
    maplist(\P^(P=X-Y, assertz(point(X-Y))), PointsByY),
    findall(Y, member(_-Y, PointsByY), Ys),
    sort(Ys, YSet),
    area(YSet, 0, Ans1),
    write_part1(Ans1).

points(_, [], []).
points(C, [instr(D,N,_)|T], [NewC|R]) :-
    move(C, D, N, NewC),
    points(NewC, T, R).

move(X-Y, DX-DY, N, NX-NY) :-
    NX #= X + N*DX,
    NY #= Y + N*DY.

area([], X, X).
area([SmallestY|Ys], Acc, Ans) :-
    % find all points with smallest Y
    findall(C, (point(C), C=_-SmallestY), Points),
    write([points,Points]), nl,
    % skip if we find no more, go to next Y level
    ( Points = [] -> area(Ys, Acc, Ans) ; 
    % sort them by X coord so that leftmost topmost is front of list
    qsort(byX, Points, Sorted),
    Sorted = [P, Q|T],
    P = PX-PY, Q = QX-PY,
    retract(point(P)),
    retract(point(Q)),
    % P and Q have same Y (guaranteed), do the following:
    % find highest point A on PX or QX projection on X-axis
    findall(A, (A=X-Y, (X=PX;X=QX), point(X-Y)), As),
    qsort(byY, As, [A|_]),
    % find point B, A's projection on either PX or QX opposite of A
    ( A=PX-AY, B=QX-AY ; A=QX-AY, B=PX-AY ),
    write([P, Q, A, B]), nl,
    % add area PQAB
    % prevent double-counting
    A=AX-AY, B=BX-AY, DY #= AY-PY+1,
    ( AX #< BX ->
        LRHC = B,
        DX #= BX-AX+1
    ;
        LRHC = A,
        DX #= AX-BX+1
    ),
    Area #= DX * DY,
    overlap(P/LRHC, Counted),
    assertz(rectangle(P/LRHC)),
    NAcc #= Acc + Area - Counted,
    write([Area, Counted]), nl,
    % A is guaranteed to already be a point but B isnt
    % add B back to points iff B wasnt a point before
    retract(point(A)),
    ( point(B) -> retract(point(B)) ; assertz(point(B)) ),
    ( T = [] -> area(Ys, NAcc, Ans) ; area([SmallestY|Ys], NAcc, Ans) ) ).

overlap(Rect, Overlap) :-
    findall(R, rectangle(R), Rects),
    overlap(Rects, Rect, Overlapping),
    maplist(assert_overlap, Overlapping),
    findall(O, overlap(O), List),
    length(List, Overlap),
    retractall(overlap(_)).
    %sum_overlap(Overlapping, Overlap).

assert_overlap((X1-Y1)/(X2-Y2)) :-
    numlist(X1, X2, Xs),
    maplist(Y1+\X^((overlap(X-Y1) -> true ; assertz(overlap(X-Y1)))), Xs),
    %maplist(Y1+\X^(assertz(overlap(X-Y1))), Xs),
    ( Y1 == Y2 -> true ;
    NewY #= Y1+1, assert_overlap((X1-NewY)/(X2-Y2)) ).


sum_overlap([], 0).
sum_overlap([(X1-Y1)/(X2-Y2)], Area) :-
    Area #= (X2-X1+1)*(Y2-Y1+1).
sum_overlap([(X1-Y1)/(X2-Y2), (X3-Y3)/(X4-Y4)], Area) :-
    Area1 #= (X2-X1+1)*(Y2-Y1+1),
    Area2 #= (X4-X3+1)*(Y4-Y3+1),
    Area #= Area1 + Area2 - 1.
sum_overlap([(X1-Y1)/(X2-Y2), (X3-Y3)/(X4-Y4), (X5-Y5)/(X6-Y6)], Area) :-
    Area1 #= (X2-X1+1)*(Y2-Y1+1),
    Area2 #= (X4-X3+1)*(Y4-Y3+1),
    Area3 #= (X6-X5+1)*(Y6-Y5+1),
    Area #= Area1 + Area2 + Area3 - 2.

overlap([], _, []).
overlap([ATL/ABR|R], BTL/BBR, Overlapping) :-
    ATL = AX1-AY1, ABR = AX2-AY2,
    BTL = BX1-BY1, BBR = BX2-BY2,
    max(AX1, BX1, Left),
    min(AX2, BX2, Right),
    max(AY1, BY1, Bottom),
    min(AY2, BY2, Top),
    ( Left #=< Right, Bottom #=< Top ->
        Overlapping = [(Left-Bottom)/(Right-Top)|T] ; Overlapping = T ),
    overlap(R, BTL/BBR, T).

max(X, Y, X) :- X #>= Y.
max(X, Y, Y) :- X #< Y.
min(X, Y, X) :- X #=< Y.
min(X, Y, Y) :- X #> Y.

parse([instr(D,N,H)|T]) --> parse_dir(D), " ", integer(N), " (#", hex(H), ")\n", parse(T).
parse([]) --> [].

parse_dir(0-(-1)) --> "U".
parse_dir(0-1)    --> "D".
parse_dir(-1-0)   --> "L".
parse_dir(1-0)    --> "R".

integer(N) --> digits(D), {number_chars(N, D)}.
digits([H|T]) --> [H], {char_type(H, decimal_digit)}, digits(T).
digits([X]) --> [X], {char_type(X, decimal_digit)}.

%hex(N) --> hexdigits(D), {number_chars(N, D)}.
hex(N) --> hexdigits(N).
hexdigits([H|T]) --> [H], {char_type(H, hexadecimal_digit)}, hexdigits(T).
hexdigits([X]) --> [X], {char_type(X, hexadecimal_digit)}.

byX(NX-NY, MX-MY, C) :-
    twostage_comp(NX-MX, NY-MY, C).
byY(NX-NY, MX-MY, C) :-
    twostage_comp(NY-MY, NX-MX, C).

twostage_comp(P1-Q1, P2-Q2, C) :-
    comp(P1, Q1, CC),
    (CC == (=) -> comp(P2, Q2, C) ; C = CC ).

comp(X, Y, >) :- X #> Y.
comp(X, Y, <) :- X #< Y.
comp(X, X, =).

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
