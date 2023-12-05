:- use_module(library(charsio)).
:- use_module(library(lambda)).

run_day(5, Filename) :-
    phrase_from_file(parse(Seeds), Filename),
    maplist(\X^Y^(find(seed, X, location, Y)), Seeds, Locations),
    sort(Locations, [Ans1|_]),
    write_part1(Ans1),
    seedranges(Seeds, SeedRanges),
    maplist(seed_to_location_ranges, SeedRanges, LocationRanges),
    append(LocationRanges, Flat),
    sort(Flat, [Ans2-_|_]),
    write_part2(Ans2).

lookup(From, X, To, Y) :-
    almanac(From, To, Dest, Source, Len),
    X #>= Source, X #< Source + Len,
    Dif #= X - Source,
    Y #= Dest + Dif.
lookup(From, X, To, X) :-
    almanac(From, To, _, _, _).

find(From, X, From, X).
find(From, X, End, Y) :-
    var(Y),
    lookup(From, X, To, N),
    find(To, N, End, Y).
find(From, X, End, Y) :-
    var(X),
    lookup(From, N, To, Y),
    find(To, X, End, N).

seedranges([], []).
seedranges([Start,Len|T], [Start-End|TT]) :-
    End #= Start + Len-1,
    seedranges(T, TT).

seed_to_location_ranges(From-To, LocRanges) :-
    transform_ranges(seed, soil, [From-To], SoilRanges),
    transform_ranges(soil, fertilizer, SoilRanges, FertRanges),
    transform_ranges(fertilizer, water, FertRanges, WaterRanges),
    transform_ranges(water, light, WaterRanges, LightRanges),
    transform_ranges(light, temperature, LightRanges, TempRanges),
    transform_ranges(temperature, humidity, TempRanges, HumRanges),
    transform_ranges(humidity, location, HumRanges, LocRanges).

transform_ranges(_, _, [], []).
transform_ranges(FromType, ToType, [From-To|T], Ranges) :-
    transform_range(FromType, ToType, From-To, R),
    transform_ranges(FromType, ToType, T, TT),
    append(R, TT, Ranges).

transform_range(FromType, ToType, From-To, Ranges) :-
    findall(Source-SourceTo/Dif, (
        almanac(FromType, ToType, Dest, Source, Len),
        SourceTo #= Source+Len-1, Dif #= Dest-Source
        ), Almanac),
    sort(Almanac, Sorted), % assume sorts by Source!
    split_range(From-To, Sorted, Ranges).

split_range(From-To, [], [From-To]).
split_range(From-To, [_-STo/_|T], Ranges) :-                                    % [SourceFrom-SourceTo] (From-To)
    From #> STo,
    split_range(From-To, T, Ranges).
split_range(From-To, [SFrom-STo/D|_], [DF-DT]) :-                               % [SourceFrom (From-To) SourceTo]
    From #>= SFrom, To #=< STo,
    DF #= From+D, DT #= To+D.
split_range(From-To, [SFrom-STo/D|T], [From-LastFrom, DF-DT|Ranges]) :-         % (From [SourceFrom To) SourceTo]
    From #< SFrom, To #=< STo, To >= SFrom,
    DF #= SFrom+D, DT #= To+D,
    LastFrom #= SFrom-1.
split_range(From-To, [SFrom-STo/D|T], [DF-DT|Ranges]) :-                        % [SourceFrom (From SourceTo] To)
    From #>= SFrom, To #> STo,
    DF #= From+D, DT #= STo+D,
    RemFrom #= STo+1,
    split_range(RemFrom-To, T, Ranges).
split_range(From-To, [SFrom-STo/D|T], [From-LastFrom, DF-DT|Ranges]) :-         % (From [SourceFrom-SourceTo] To)
    From #< SFrom, To #> STo,
    DF #= SFrom+D, DT #= STo+D,
    LastFrom #= SFrom-1, RemFrom #= STo+1,
    split_range(RemFrom-To, T, Ranges).
split_range(From-To, [SFrom-_/_|_], [From-To]) :-                               % (From-To) [SourceFrom-SourceTo]
    To #< SFrom.

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
