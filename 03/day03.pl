:- use_module(library(charsio)).

run_day(3, Filename) :-
    phrase_from_file(parse(0,0), Filename),
    findall(N-X-Z-Y, number(N, X-Z, Y), Numbers),
    maplist(part_number, Numbers, PartNumbers),
    sum(PartNumbers, #=, Ans1),
    write_part1(Ans1),
    findall(X-Y, symbol('*', X, Y), GearSymbols),
    maplist(gear_ratio, GearSymbols, Ratios),
    sum(Ratios, #=, Ans2),
    write_part2(Ans2).

adjacent(_-FromX-ToX-Y, SX-SY) :-
    SX #>= FromX - 1,
    SX #=< ToX + 1,
    SY #=< Y+1,
    SY #>= Y-1.

% if P is not a proper part number, N = 0
part_number(P, N) :-
    ( (symbol(_,SX,SY), adjacent(P,SX-SY)) -> 
        P = N-_-_-_
        ; N #= 0
        ).

% if X-Y is not a proper gear, Ratio = 0
gear_ratio(X-Y, Ratio) :-
    findall(N, (number(N, NX-NZ, NY), adjacent(N-NX-NZ-NY, X-Y)), AdjacentNums),
    ( AdjacentNums = [A,B] -> 
        Ratio #= A*B
        ; Ratio #= 0
        ).

parse(X,Y) --> ".", {Z #= X+1}, parse(Z,Y).
parse(_,Y) --> "\n", {Z #= Y+1}, parse(0,Z).
parse(X,Y) --> integer(N, L), {Z #= X+L, To #= Z-1, assertz(number(N, X-To, Y))}, parse(Z,Y).
parse(X,Y) --> [S], {Z #= X+1, assertz(symbol(S, X, Y))}, parse(Z,Y).
parse(_,_) --> [].

integer(N, L) --> digits(D), {number_chars(N, D), length(D, L)}.
digits([H|T]) --> [H], {char_type(H, decimal_digit)}, digits(T).
digits([X]) --> [X], {char_type(X, decimal_digit)}.