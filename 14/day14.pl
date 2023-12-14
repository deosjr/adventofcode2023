:- use_module(library(lambda)).

:- dynamic(seen/2).

run_day(14, Filename) :-
    phrase_from_file(parse(List), Filename),
    % maplist(reverse) . transpose == rotate 90 counterclockwise
    % so that 'north' is left and we can start at sliding westwards
    maplist(reverse, List, Rev),
    transpose(Rev, CCW),
    slide_west(CCW, S),
    maplist(count_load, S, C),
    sum(C, #=, Ans1),
    write_part1(Ans1),
    cycles(0, CCW, Start, Len),
    Step #= Start + ((1000000000-Start) mod Len),
    seen(Final, Step),
    maplist(count_load, Final, Cs),
    sum(Cs, #=, Ans2),
    write_part2(Ans2).

cycles(N, List, CycleStart, CycleLen) :-
    ( seen(List, X) -> CycleStart #= X, CycleLen #= N-X
    ; cycle(List, C), assertz(seen(List, N)), M #= N+1, cycles(M, C, CycleStart, CycleLen) ).

cycle(List, Out) :-
    slide_west(List, A),
    rotate(A, AR),
    slide_west(AR, B),
    rotate(B, BR),
    slide_west(BR, C),
    rotate(C, CR),
    slide_west(CR, D),
    rotate(D, Out).

% rotate 90 degrees clockwise
rotate([H|T], Out) :-
    length(H, L),
    length(Empties, L),
    maplist(=([]), Empties),
    rotate([H|T], Empties, Out).

rotate([], X, X).
rotate([H|T], Acc, Out) :-
    zip(H, Acc, NAcc),
    rotate(T, NAcc, Out).

zip([], [], []).
zip([X|T1], [Y|T2], [[X|Y]|Z]) :-
    zip(T1, T2, Z).

% slide_west(NumEmpty, NumRock, In, Out)
slide_west(List, Out) :-
    maplist(slide_west(0, 0), List, Out).
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
