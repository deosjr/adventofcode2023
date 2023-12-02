:- use_module(library(charsio)).
:- use_module(library(reif)).

run_day(2, Filename) :-
    phrase_from_file(parse(List), Filename),
    maplist(possible, List, IDs),
    sum(IDs, #=, Ans1),
    write_part1(Ans1),
    maplist(power, List, Powers),
    sum(Powers, #=, Ans2),
    write_part2(Ans2).

possible(ID-List, N) :-
    ( tfilter(possible, List, List) -> N#=ID ; N#=0 ).

% reify truth to use in tfilter
possible([], true).
possible([H|T], Truth) :-
    H = C-N,
    if_(possible(C, N)
        , possible(T, Truth)
        , Truth = false
        ).

%the bag contained only 12 red cubes, 13 green cubes, and 14 blue cubes
possible(red, N, T) :-
    ( N #> 12 -> T = false ; T = true ).
possible(green, N, T) :-
    ( N #> 13 -> T = false ; T = true ).
possible(blue, N, T) :-
    ( N #> 14 -> T = false ; T = true ).

power(_-List, N) :-
    append(List, Flat),
    find(red, Flat, R),
    find(green, Flat, G),
    find(blue, Flat, B),
    N #= R*G*B.
    
find(Color, List, N) :-
    findall(X, member(Color-X, List), Nums),
    sort(Nums, Sorted),
    reverse(Sorted, [N|_]).

parse([ID-S|T]) --> "Game ", integer(ID), ": ", subsets(S), parse(T).
parse([]) --> [].

integer(N) --> digits(D), {number_chars(N, D)}.
digits([H|T]) --> [H], {char_type(H, decimal_digit)}, digits(T).
digits([X]) --> [X], {char_type(X, decimal_digit)}.

subsets([H|T]) --> subset(H), "; ", subsets(T).
subsets([S]) --> subset(S), "\n".

subset([C-N|T]) --> integer(N), " ", color(C), ", ", subset(T).
subset([C-N]) --> integer(N), " ", color(C).

color(red) --> "red".
color(blue) --> "blue".
color(green) --> "green".
