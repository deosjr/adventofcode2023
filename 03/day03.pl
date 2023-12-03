:- use_module(library(charsio)).

run_day(3, Filename) :-
    phrase_from_file(parse(0,0), Filename),
    part1,
    part2.

part1 :-
    findall(N, (
        number(N, X-Z, Y),
        part_number(N-X-Z-Y)
        ), PartNumbers),
    sum(PartNumbers, #=, Ans1),
    write_part1(Ans1).

part2 :-
    findall(Ratio, (
        symbol('*', X, Y),
        gear_ratio(X-Y, Ratio)
        ), Ratios),
    sum(Ratios, #=, Ans2),
    write_part2(Ans2).

adjacent(_-FromX-ToX-Y, SX-SY) :-
    SX #>= FromX - 1,
    SX #=< ToX + 1,
    SY #=< Y+1,
    SY #>= Y-1.

part_number(P) :-
    symbol(_,SX,SY),
    adjacent(P,SX-SY).

gear_ratio(X-Y, Ratio) :-
    findall(N, (number(N, NX-NZ, NY), adjacent(N-NX-NZ-NY, X-Y)), [A,B]),
    Ratio #= A*B.

parse(X,Y) --> ".", {Z #= X+1}, parse(Z,Y).
parse(_,Y) --> "\n", {Z #= Y+1}, parse(0,Z).
parse(X,Y) --> integer(N, L), {Z #= X+L, To #= Z-1, assertz(number(N, X-To, Y))}, parse(Z,Y).
parse(X,Y) --> [S], {Z #= X+1, assertz(symbol(S, X, Y))}, parse(Z,Y).
parse(_,_) --> [].

integer(N, L) --> digits(D), {number_chars(N, D), length(D, L)}.
digits([H|T]) --> [H], {char_type(H, decimal_digit)}, digits(T).
digits([X]) --> [X], {char_type(X, decimal_digit)}.
