:- use_module(library(charsio)).
:- use_module(library(ordsets)).

run_day(4, Filename) :-
    phrase_from_file(parse(List), Filename),
    maplist(part1, List, Sums),
    sum(Sums, #=, Ans1),
    write_part1(Ans1).

part1(Win-Have, N) :-
    list_to_ord_set(Win, WinSet),
    list_to_ord_set(Have, HaveSet),
    ord_intersection(WinSet, HaveSet, Overlap),
    ( Overlap = [] -> N #= 0
        ; length(Overlap, L), N #= 2 ^ (L-1)
        ).

parse([Win-Have|T]) --> "Card", spaces, integer(_ID), ": ", intlist(Win), " | ", intlist(Have), "\n", parse(T).
parse([]) --> [].

intlist([N|T]) --> spaces, integer(N), intlist(T).
intlist([]) --> [].

spaces --> " ", spaces.
spaces --> [].

integer(N) --> digits(D), {number_chars(N, D)}.
digits([H|T]) --> [H], {char_type(H, decimal_digit)}, digits(T).
digits([X]) --> [X], {char_type(X, decimal_digit)}.
