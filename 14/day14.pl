:- use_module(library(lambda)).

run_day(14, Filename) :-
    phrase_from_file(parse(List), Filename),
    transpose(List, T),
    maplist(slide_west(0,0), T, S),
    maplist(count_load, S, C),
    sum(C, #=, Ans1),
    write(Ans1).
    %maplist(\X^(write(X),nl), T).

% slide_west(NumEmpty, NumRock, In, Out)
slide_west(N, M, [], Out) :-
    assemble(N, M, Out).
slide_west(N, M, ['.'|T], Out) :-
    NN #= N+1,
    slide_west(NN, M, T, Out).
slide_west(N, M, ['O'|T], Out) :-
    MM #= M+1,
    slide_west(N, MM, T, Out).
slide_west(N, M, ['#'|T], Out) :-
    assemble(N, M, A),
    slide_west(0, 0, T, O),
    append(A, "#", TEST),  % again I dont understand why append(A, ['#'|O], Out) fails
    append(TEST, O, Out).

assemble(N, M, Out) :-
    length(Empty, N),
    length(Rocks, M),
    maplist(=('.'), Empty),
    maplist(=('O'), Rocks),
    append(Rocks, Empty, Out).

count_load(List, N) :-
    length(List, L),
    count(List, L, N).

count([], _, 0).
count([C|T], X, Y) :-
    XX #= X-1,
    count(T, XX, Z),
    ( C = 'O' -> Y #= Z+X ; Y #= Z ).

parse([L|T]) --> line(L), parse(T).
parse([]) --> [].

line([]) --> "\n".
line([H|T]) --> [H], {H\=='\n'}, line(T).
