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

parse_replace('1'), "e" --> "one".
parse_replace('2'), "o" --> "two".
parse_replace('3'), "e" --> "three".
parse_replace('4'), "r" --> "four".
parse_replace('5'), "e" --> "five".
parse_replace('6'), "x" --> "six".
parse_replace('7'), "n" --> "seven".
parse_replace('8'), "t" --> "eight".
parse_replace('9'), "e" --> "nine".
parse_replace(H) --> [H], { char_type(H,alphanumeric) }.

parse_line([], X, X).
parse_line([H|T], X, DX) :-
    phrase(parse_replace(H), X, DY),
    parse_line(T, DY, DX).

parse2([]) --> [].
parse2([H|T]) --> parse_line(H), "\n", parse2(T).
