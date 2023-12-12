:- use_module(library(charsio)).

run_day(12, Filename) :-
    phrase_from_file(parse(List), Filename),
    maplist(arrangements, List, A),
    sum(A, #=, Ans1),
    write_part1(Ans1).

arrangements(Springs-Groups, N) :-
    findall(_, arrangement(Springs, Groups), List),
    length(List, N).

arrangement(Springs, Groups) :-
    arrangement(Springs, 0, Groups).

arrangement([], 0, []).
arrangement([], N, [N]) :- N #>0.
arrangement(['.'|T], 0, Groups) :-
    arrangement(T, 0, Groups).
arrangement(['.'|T], N, [N|Groups]) :-
    N #> 0,
    arrangement(T, 0, Groups).
arrangement(['#'|T], N, Groups) :-
    M #= N+1,
    arrangement(T, M, Groups).
arrangement(['?'|T], N, Groups) :-
    arrangement(['.'|T], N, Groups).
arrangement(['?'|T], N, Groups) :-
    arrangement(['#'|T], N, Groups).

parse([S-N|T]) --> springs(S), intlist(N), parse(T).
parse([]) --> [].

springs([H|T]) --> [H], {char_type(H, ascii_punctuation)}, springs(T).
springs([]) --> " ".

intlist([N|T]) --> integer(N), ",", intlist(T).
intlist([N]) --> integer(N), "\n".

integer(N) --> digits(D), {number_chars(N, D)}.
digits([H|T]) --> [H], {char_type(H, decimal_digit)}, digits(T).
digits([X]) --> [X], {char_type(X, decimal_digit)}.
