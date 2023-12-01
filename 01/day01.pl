:- use_module(library(charsio)).

run_day(1, Filename) :-
    phrase_from_file(parse(List), Filename),
    maplist(calibrate, List, Nums),
    sum(Nums, #=, Ans1),
    write_part1(Ans1),
    phrase_from_file(parse2(List2), Filename),
    maplist(calibrate, List2, Nums2),
    sum(Nums2, #=, Ans2),
    write_part2(Ans2).

calibrate(List, Num) :-
    reverse(List, Rev),
    firstdigit(List, First),
    firstdigit(Rev, Last),
    number_chars(Num, [First, Last]).

firstdigit([H|T], X) :-
    ( char_type(H, decimal_digit) ->
        H = X ;
        firstdigit(T, X)
    ).

parse([H|T]) --> alphanums(H), "\n", parse(T).
parse([X]) --> alphanums(X), "\n".

alphanums([H|T]) --> [H], {char_type(H, alphanumeric)}, alphanums(T).
alphanums([]) --> [].

% these doubles wrecked me, this is the lazy approach to fixing it once I realised
parse_replace(X) --> "twone",  {number_chars(2, C2), number_chars(1, C1), append(C2, C1, C), append(C, T, X)}, parse_replace(T).
parse_replace(X) --> "eightwo",  {number_chars(8, C8), number_chars(2, C2), append(C8, C2, C), append(C, T, X)}, parse_replace(T).
parse_replace(X) --> "eighthree",  {number_chars(8, C8), number_chars(3, C3), append(C8, C3, C), append(C, T, X)}, parse_replace(T).
parse_replace(X) --> "sevenine",  {number_chars(7, C7), number_chars(9, C9), append(C7, C9, C), append(C, T, X)}, parse_replace(T).
parse_replace(X) --> "oneight",  {number_chars(1, C1), number_chars(8, C8), append(C1, C8, C), append(C, T, X)}, parse_replace(T).
% parse_replace(['1'|T]) wasnt working, this should be simpler somehow
parse_replace(X) --> "one",   {number_chars(1, C), append(C, T, X)}, parse_replace(T).
parse_replace(X) --> "two",   {number_chars(2, C), append(C, T, X)}, parse_replace(T).
parse_replace(X) --> "three", {number_chars(3, C), append(C, T, X)}, parse_replace(T).
parse_replace(X) --> "four",  {number_chars(4, C), append(C, T, X)}, parse_replace(T).
parse_replace(X) --> "five",  {number_chars(5, C), append(C, T, X)}, parse_replace(T).
parse_replace(X) --> "six",   {number_chars(6, C), append(C, T, X)}, parse_replace(T).
parse_replace(X) --> "seven", {number_chars(7, C), append(C, T, X)}, parse_replace(T).
parse_replace(X) --> "eight", {number_chars(8, C), append(C, T, X)}, parse_replace(T).
parse_replace(X) --> "nine",  {number_chars(9, C), append(C, T, X)}, parse_replace(T).
parse_replace([H|T]) --> [H], { char_type(H,alphanumeric) }, parse_replace(T).
parse_replace([]) --> "\n".

parse2([H|T]) --> parse_replace(H), parse2(T).
parse2([]) --> [].
