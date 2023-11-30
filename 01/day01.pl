use_test_input.

run_day(1, Filename) :-
    phrase_from_file(parse(X), Filename),
    write_part1(X).

parse(X) --> "abc", "\n", {X = 42}.
