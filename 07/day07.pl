:- use_module(library(charsio)).
:- use_module(library(lists)).

%use_test_input.

run_day(7, Filename) :-
    phrase_from_file(parse, Filename),
    findall(H-T-B, (hand(H, B), qsort(cardcomp,H,S), type(S, T)), Hands),
    qsort(hand_strength, Hands, Ranked),
    length(Ranked, L),
    score(Ranked, L, Ans1),
    write_part1(Ans1).

score([], _, 0).
score([_-_-Bid|T], N, Ans) :-
    X #= Bid * N,
    M #= N - 1,
    score(T, M, Out),
    Ans #= Out + X.

cards("AKQJT98765432").

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

cardcomp(X, Y, T) :-
    cards(C),
    nth0(N, C, X),
    nth0(M, C, Y),
    reifycomp(N, M, T).

reifycomp(N, M, >) :- N #> M.
reifycomp(N, M, <) :- N #< M.
reifycomp(N, N, =).

hand_strength(Hand1-T1-_, Hand2-T2-_, C) :-
    handcomp(Hand1-T1, Hand2-T2, C).

handcomp(_-T1, _-T2, >) :-
    typecomp(T1, T2, >).
handcomp(_-T1, _-T2, <) :-
    typecomp(T1, T2, <).
handcomp(H1-T1, H2-T2, C) :-
    typecomp(T1, T2, =),
    highcard(H1, H2, C).

highcard([], [], =).
highcard([H1|_], [H2|_], >) :-
    cardcomp(H1, H2, >).
highcard([H1|_], [H2|_], <) :-
    cardcomp(H1, H2, <).
highcard([H1|T1], [H2|T2], C) :-
    cardcomp(H1, H2, =),
    highcard(T1, T2, C).

types([five, four, fullhouse, three, two, one, high]).

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
    types(T),
    nth0(N, T, T1),
    nth0(M, T, T2),
    reifycomp(N, M, C).

parse --> cardchars(H), " ", integer(B), "\n",
    {assertz(hand(H, B))}, parse.
parse --> [].

cardchars([H|T]) --> [H], {cards(C), memberchk(H,C)}, cardchars(T).
cardchars([]) --> [].

integer(N) --> digits(D), {number_chars(N, D)}.
digits([H|T]) --> [H], {char_type(H, decimal_digit)}, digits(T).
digits([X]) --> [X], {char_type(X, decimal_digit)}.
