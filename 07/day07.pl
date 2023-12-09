:- use_module(library(charsio)).
:- use_module(library(lists)).

run_day(7, Filename) :-
    phrase_from_file(parse, Filename),
    findall(H-T-B, (hand(H, B), qsort(cards_p1_comp,H,S), type(S, T)), Hands),
    qsort(hand_strength(cards_p1_comp), Hands, Ranked),
    length(Ranked, L),
    score(Ranked, L, Ans1),
    write_part1(Ans1),
    findall(H-T-B, (hand(H, B), qsort(cards_p2_comp,H,S), type2(S, T)), Hands2),
    qsort(hand_strength(cards_p2_comp), Hands2, Ranked2),
    length(Ranked2, L2),
    score(Ranked2, L2, Ans2),
    write_part2(Ans2).

score([], _, 0).
score([_-_-Bid|T], N, Ans) :-
    X #= Bid * N,
    M #= N - 1,
    score(T, M, Out),
    Ans #= Out + X.

cards_p1("AKQJT98765432").
cards_p2("AKQT98765432J").

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

cards_p1_comp(X, Y, T) :-
    cards_p1(C), listcomp(C, X, Y, T).
cards_p2_comp(X, Y, T) :-
    cards_p2(C), listcomp(C, X, Y, T).

listcomp(List, X, Y, T) :-
    nth0(N, List, X),
    nth0(M, List, Y),
    reifycomp(N, M, T).

reifycomp(N, M, >) :- N #> M.
reifycomp(N, M, <) :- N #< M.
reifycomp(N, N, =).

hand_strength(CompPred, Hand1-T1-_, Hand2-T2-_, C) :-
    handcomp(CompPred, Hand1-T1, Hand2-T2, C).

handcomp(Pred, H1-T1, H2-T2, C) :-
    typecomp(T1, T2, Comp),
    ( Comp == (=) -> highcard(Pred, H1, H2, C) ; C=Comp ).

highcard(_, [], [], =).
highcard(Pred, [H1|T1], [H2|T2], C) :-
    call(Pred, H1, H2, Comp),
    ( Comp == (=) -> highcard(Pred, T1, T2, C) ; C=Comp ).

types([five, four, fullhouse, three, two, one, high]).

type2([A,B,C,D,E], T) :-
    E \= 'J', type([A,B,C,D,E], T).
type2([A,B,C,D,'J'], T) :-
    D \= 'J', type([A,B,C,D,'X'], Out),
    transform_type(1, Out, T).
type2([A,B,C,'J','J'], T) :-
    C \= 'J', type([A,B,C,'X','Y'], Out),
    transform_type(2, Out, T).
type2([A,B,'J','J','J'], T) :-
    B \= 'J', type([A,B,'X','Y','Z'], Out),
    transform_type(3, Out, T).
type2([_,'J','J','J','J'], five).

transform_type(1, high, one).
transform_type(1, one, three).
transform_type(1, two, fullhouse).
transform_type(1, three, four).
transform_type(1, four, five).
transform_type(2, high, three).
transform_type(2, one, four).
transform_type(2, three, five).
transform_type(3, high, four).
transform_type(3, one, five).

type([X,X,X,X,X], five).
type([X,X,X,X,Y], four) :- X \= Y.
type([Y,X,X,X,X], four) :- X \= Y.
type([X,X,X,Y,Y], fullhouse) :- X \= Y.
type([Y,Y,X,X,X], fullhouse) :- X \= Y.
type([X,X,X,Y,Z], three) :- X \= Y, Y \= Z.
type([Y,X,X,X,Z], three) :- X \= Y, X \= Z.
type([Y,Z,X,X,X], three) :- X \= Z, Y \= Z.
type([X,X,Y,Y,Z], two) :- X \= Y, Y \= Z.
type([X,X,Z,Y,Y], two) :- X \= Z, Y \= Z.
type([Z,X,X,Y,Y], two) :- X \= Z, Y \= X.
type([X,X,A,B,C], one) :- X \= A, A \= B, B \= C.
type([A,X,X,B,C], one) :- X \= A, X \= B, B \= C.
type([A,B,X,X,C], one) :- A \= B, X \= B, X \= C.
type([A,B,C,X,X], one) :- A \= B, B \= C, X \= C.
type([A,B,C,D,E], high) :- A \= B, B \= C, C \= D, D \= E.

typecomp(T1, T2, C) :-
    types(T), listcomp(T, T1, T2, C).

parse --> cardchars(H), " ", integer(B), "\n",
    {assertz(hand(H, B))}, parse.
parse --> [].

cardchars([H|T]) --> [H], {cards_p1(C), memberchk(H,C)}, cardchars(T).
cardchars([]) --> [].

integer(N) --> digits(D), {number_chars(N, D)}.
digits([H|T]) --> [H], {char_type(H, decimal_digit)}, digits(T).
digits([X]) --> [X], {char_type(X, decimal_digit)}.
