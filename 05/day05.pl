:- use_module(library(charsio)).
:- use_module(library(lambda)).

%use_test_input.

run_day(5, Filename) :-
    phrase_from_file(parse(Seeds), Filename),
    maplist(\X^Y^(find(seed, X, location, Y)), Seeds, Locations),
    sort(Locations, [Ans1|_]),
    write(Ans1).

lookup(From, X, To, Y) :-
    almanac(From, To, Dest, Source, Len),
    X #>= Source, X #< Source + Len,
    Dif #= X - Source,
    Y #= Dest + Dif.

transform(From, X, To, Y) :-
    ( lookup(From, X, To, N) ->
        Y #= N ;
        lookup(From, _, To, _),
        Y #= X
        ).

find(From, X, From, X).
find(From, X, End, Y) :-
    transform(From, X, To, N),
    find(To, N, End, Y).

parse(Seeds) --> "seeds: ", intlist(Seeds), "\n", parse_map.

parse_map --> "\n", name(From), "-to-", name(To), " map:\n", parse_range(From, To).
parse_map --> [].

parse_range(From, To) --> intlist([Dest, Source, Len]), "\n",
    {assertz(almanac(From,To,Dest,Source,Len))}, parse_range(From, To).
parse_range(_,_) --> parse_map.
parse_range(_,_) --> [].

name(seed) --> "seed".
name(soil) --> "soil".
name(fertilizer) --> "fertilizer".
name(water) --> "water".
name(light) --> "light".
name(temperature) --> "temperature".
name(humidity) --> "humidity".
name(location) --> "location".

intlist([N|T]) --> spaces, integer(N), intlist(T).
intlist([]) --> [].

spaces --> " ", spaces.
spaces --> [].

integer(N) --> digits(D), {number_chars(N, D)}.
digits([H|T]) --> [H], {char_type(H, decimal_digit)}, digits(T).
digits([X]) --> [X], {char_type(X, decimal_digit)}.
