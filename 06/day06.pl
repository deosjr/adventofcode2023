:- use_module(library(between)).
:- use_module(library(charsio)).
:- use_module(library(lambda)).

run_day(6, Filename) :-
    phrase_from_file(parse(Times, Distances), Filename),
    maplist(\X^Y^Z^(Z=X-Y), Times, Distances, Zipped),
    maplist(solve, Zipped, Ways),
    foldl(\X^Y^Z^(Z#=X*Y), Ways, 1, Ans1),
    write_part1(Ans1),
    phrase_from_file(parse(Time-Distance), Filename),
    solve(Time-Distance, Ans2),
    write_part2(Ans2).

solve(Time-Distance, L) :-
    T2 #= Time*Time,
    Sqrt is ceiling(sqrt(T2 - 4*Distance)),
    C #= (-Time + Sqrt) // -2,
    L #= Time+1 - (2*(C+1)).

parse(T,D) --> "Time:", intlist(T), "\nDistance:", intlist(D), "\n".

parse(T-D) --> "Time:", int_with_spaces(T), "\nDistance:", int_with_spaces(D), "\n".

intlist([N|T]) --> spaces, integer(N), intlist(T).
intlist([]) --> [].

spaces --> " ", spaces.
spaces --> [].

integer(N) --> digits(D), {number_chars(N, D)}.
digits([H|T]) --> [H], {char_type(H, decimal_digit)}, digits(T).
digits([X]) --> [X], {char_type(X, decimal_digit)}.

int_with_spaces(N) --> digits_with_spaces(D), {number_chars(N, D)}.
digits_with_spaces(T) --> " ", digits_with_spaces(T).
digits_with_spaces([H|T]) --> [H], {char_type(H, decimal_digit)}, digits_with_spaces(T).
digits_with_spaces([X]) --> [X], {char_type(X, decimal_digit)}.
