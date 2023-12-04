:- use_module(library(between)).
:- use_module(library(charsio)).
:- use_module(library(lambda)).
:- use_module(library(ordsets)).

run_day(4, Filename) :-
    phrase_from_file(parse, Filename),
    findall(Win-Have, card(_, Win, Have), Cards),
    maplist(part1, Cards, Sums),
    sum(Sums, #=, Ans1),
    write_part1(Ans1),
    length(Cards, NumCards),
    numlist(NumCards, IDs),
    maplist(\X^Y^(Y=X-1), IDs, Start),
    part2(Start, 0, Ans2),
    write_part2(Ans2).

overlap(Win-Have, L) :-
    list_to_ord_set(Win, WinSet),
    list_to_ord_set(Have, HaveSet),
    ord_intersection(WinSet, HaveSet, Overlap),
    length(Overlap, L).

score(L, N) :- ( L #= 0 -> N #= 0 ; N #= 2 ^ (L-1) ).

part1(Card, N) :-
    overlap(Card, L),
    score(L, N).

part2([], Ans, Ans).
part2([ID-N|T], Acc, Ans) :-
    card(ID, Win, Have),
    overlap(Win-Have, L),
    add_cards(L, N, T, NewT),
    NAcc #= Acc + N,
    part2(NewT, NAcc, Ans).

add_cards(0, _, T, T).
add_cards(_, _, [], []).
add_cards(N, M, [ID-X|T], [ID-Y|NewT]) :-
    Y #= X + M,
    NN #= N - 1,
    add_cards(NN, M, T, NewT).

parse --> "Card", spaces, integer(ID), ": ", intlist(Win), " | ", intlist(Have), "\n", {assertz(card(ID,Win,Have))}, parse.
parse --> [].

intlist([N|T]) --> spaces, integer(N), intlist(T).
intlist([]) --> [].

spaces --> " ", spaces.
spaces --> [].

integer(N) --> digits(D), {number_chars(N, D)}.
digits([H|T]) --> [H], {char_type(H, decimal_digit)}, digits(T).
digits([X]) --> [X], {char_type(X, decimal_digit)}.
