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
parse_replace(X) --> "twone",  {append("21", T, X)}, parse_replace(T).
parse_replace(X) --> "eightwo",  {append("82", T, X)}, parse_replace(T).
parse_replace(X) --> "eighthree",  {append("83", T, X)}, parse_replace(T).
parse_replace(X) --> "sevenine",  {append("79", T, X)}, parse_replace(T).
parse_replace(X) --> "oneight",  {append("18", T, X)}, parse_replace(T).
parse_replace(X) --> "threeight",  {append("38", T, X)}, parse_replace(T).
parse_replace(X) --> "fiveight",  {append("58", T, X)}, parse_replace(T).
% parse_replace(['1'|T]) wasnt working, this should be simpler somehow
parse_replace(X) --> "one",   {append("1", T, X)}, parse_replace(T).
parse_replace(X) --> "two",   {append("2", T, X)}, parse_replace(T).
parse_replace(X) --> "three", {append("3", T, X)}, parse_replace(T).
parse_replace(X) --> "four",  {append("4", T, X)}, parse_replace(T).
parse_replace(X) --> "five",  {append("5", T, X)}, parse_replace(T).
parse_replace(X) --> "six",   {append("6", T, X)}, parse_replace(T).
parse_replace(X) --> "seven", {append("7", T, X)}, parse_replace(T).
parse_replace(X) --> "eight", {append("8", T, X)}, parse_replace(T).
parse_replace(X) --> "nine",  {append("9", T, X)}, parse_replace(T).
parse_replace([H|T]) --> [H], { char_type(H,alphanumeric) }, parse_replace(T).
parse_replace([]) --> "\n".

parse2([H|T]) --> parse_replace(H), parse2(T).
parse2([]) --> [].
