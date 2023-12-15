run_day(15, Filename) :-
    phrase_from_file(parse(List), Filename),
    maplist(hash, List, Hashes),
    sum(Hashes, #=, Ans1),
    write_part1(Ans1).

hash(List, Hash) :-
    hash(List, 0, Hash).

hash([], X, X).
hash([H|T], Acc, N) :-
    char_code(H, C),
    NAcc #= (17 * (Acc + C)) mod 256,
    hash(T, NAcc, N).

parse([H|T]) --> parse_step(H), ",", parse(T).
parse([H]) --> 
    parse_step(H), "\n".

parse_step([C|T]) -->
    [C],
    { [C] \== "," }, % because C \== ',' breaks...
    parse_step(T).

parse_step([]) --> [].
