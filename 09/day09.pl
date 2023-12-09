:- use_module(library(charsio)).
:- use_module(library(lambda)).

run_day(9, Filename) :-
    phrase_from_file(parse(List), Filename),
    maplist(reverse, List, Rev),
    solve(Rev, Ans1),
    write_part1(Ans1),
    solve(List, Ans2),
    write_part2(Ans2).

solve(List, Ans) :-
    maplist(predict, List, Predicted),
    sum(Predicted, #=, Ans).

predict(List, Prediction) :-
    predict(List, [], Prediction).

predict(List, Lasts, Out) :-
    List = [H|_],
    ( all_equal(List) ->
        extrapolate(H, Lasts, Out)
    ;
        reduce(List, Reduced),
        predict(Reduced, [H|Lasts], Out)
    ).

extrapolate(N, List, Out) :-
    foldl(\X^Y^Z^(Z#=X+Y), List, N, Out).

reduce([A,B], [X]) :- X #= A-B.
reduce([A,B,C|T], [X|R]) :-
    X #= A-B,
    reduce([B,C|T], R).

all_equal([H|T]) :-
    maplist(=(H), T).

parse([H|T]) --> intlist(H), "\n", parse(T).
parse([]) --> [].

intlist([N|T]) --> spaces, integer(N), intlist(T).
intlist([]) --> [].

spaces --> " ", spaces.
spaces --> [].

integer(N) --> "-", digits(D), {number_chars(N, ['-'|D])}.
integer(N) --> digits(D), {number_chars(N, D)}.
digits([H|T]) --> [H], {char_type(H, decimal_digit)}, digits(T).
digits([X]) --> [X], {char_type(X, decimal_digit)}.
