:- use_module(library(between)).
:- use_module(library(charsio)).
:- use_module(library(lambda)).
:- use_module(library(reif)).

%use_test_input.

run_day(6, Filename) :-
    phrase_from_file(parse(Times, Distances), Filename),
    maplist(\X^Y^Z^(Z=X-Y), Times, Distances, Zipped),
    maplist(part1, Zipped, Ways),
    foldl(\X^Y^Z^(Z#=X*Y), Ways, 1, Ans1),
    write_part1(Ans1).

part1(T-D, L) :-
    numlist(0, T, Millis),
    maplist(D+\X^Y^(Y#=X*(T-X)), Millis, Ds),
    tfilter(beats(D), Ds, Wins),
    length(Wins, L).

beats(X, Y, T) :- ( Y #>X -> T = true ; T = false ).

parse(T,D) --> "Time:", intlist(T), "\nDistance:", intlist(D), "\n".

intlist([N|T]) --> spaces, integer(N), intlist(T).
intlist([]) --> [].

spaces --> " ", spaces.
spaces --> [].

integer(N) --> digits(D), {number_chars(N, D)}.
digits([H|T]) --> [H], {char_type(H, decimal_digit)}, digits(T).
digits([X]) --> [X], {char_type(X, decimal_digit)}.
