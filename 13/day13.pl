run_day(13, Filename) :-
    phrase_from_file(parse(List), Filename),
    maplist(find_mirror(prefix), List, Ns),
    sum(Ns, #=, Ans1),
    write_part1(Ans1),
    maplist(find_mirror(prefix_diff(1)), List, Ms),
    sum(Ms, #=, Ans2),
    write_part2(Ans2).

find_mirror(Pred, List, N) :-
    ( find_mirror(Pred, List, [], X) -> N #= 100*X
    ; transpose(List, T), find_mirror(Pred, T, [], N) ).

find_mirror(Pred, [A,B|T], List, N) :-
    ( is_mirror(Pred, [A|List], [B|T]) -> length([A|List], N) ; find_mirror(Pred, [B|T], [A|List], N) ).

is_mirror(Pred, List, T) :-
    call(Pred, List, T) ; call(Pred, T, List).

prefix(Prefix, List) :-
    append(Prefix, _, List).

prefix_diff(0, [], _).
prefix_diff(N, [X|A], [Y|B]) :-
    diff(X, Y, Z),
    prefix_diff(M, A, B),
    N #= M + Z.

diff([],[], 0).
diff([X|A], [X|B], N) :-
    diff(A, B, N).
diff([X|A], [Y|B], N) :-
    X \== Y,
    diff(A, B, M),
    N #= M+1.

parse([P|T]) --> pattern(P), "\n", parse(T).
parse([P]) --> pattern(P).

pattern([L|T]) --> line(L), {L\==[]}, pattern(T).
pattern([]) --> [].

line([]) --> "\n".
line(X) --> "#", {append("#",T,X)}, line(T).
line(X) --> ".", {append(".",T,X)}, line(T).
