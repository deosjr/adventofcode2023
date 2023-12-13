:- use_module(library(lists)).

run_day(13, Filename) :-
    phrase_from_file(parse(List), Filename),
    maplist(find_mirror, List, Ns),
    sum(Ns, #=, Ans1),
    write_part1(Ans1).

find_mirror(List, N) :-
    ( find_mirror(List, [], X) -> N #= 100*X
    ; transpose(List, T), find_mirror(T, [], N) ).

find_mirror([A,B|T], List, N) :-
    A \= B, find_mirror([B|T], [A|List], N).
find_mirror([X,X|T], List, N) :-
    ( is_mirror(List, T) -> length([X|List], N) ; find_mirror([X|T], [X|List], N) ).

is_mirror(List, T) :-
    prefix(List, T) ; prefix(T, List).

prefix(Prefix, List) :-
    append(Prefix, _, List).

parse([P|T]) --> pattern(P), "\n", parse(T).
parse([P]) --> pattern(P).

pattern([L|T]) --> line(L), {L\==[]}, pattern(T).
pattern([]) --> [].

line([]) --> "\n".
line(X) --> "#", {append("#",T,X)}, line(T).
line(X) --> ".", {append(".",T,X)}, line(T).
