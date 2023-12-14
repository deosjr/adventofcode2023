%:- use_module(library(tabling)).
%:- table arrangements/2.
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).

:- dynamic(mem/3).

run :-
    %arrangements([['?','?','?','#']], [1], N), write(N), nl.
    %arrangement(['?', '?', '#', '?'], [1, 1], N), write(N), nl,
    %length(TEST, 4),
    %arrangement(TEST, [1,1], N), write(TEST-N),nl, fail,
    phrase_from_file(parse(List), "12/day12.input"),
    %phrase_from_file(parse(List), "12/test"),
    solve(List, Ans1),
    write(Ans1), nl,
    maplist([X,Y]>>(X=S-G, quintuple_join(S,S5), quintuple(G,G5), Y=S5-G5), List, Quints),
    solve(Quints, Ans2),
    write(Ans2), nl.

solve(List, Ans) :-
    maplist([X,Y]>>(X=S-G,simplify([], S, Simple),Y=Simple-G), List, Simplified),
    maplist([X,Y]>>(X=S-G, arrangements(S,G,Y)), Simplified, Nums),
    sum(Nums, #=, Ans).

    %arrangements("??"-[1], N), write(N), nl.

    /*
    Simplified = [_,S-G|_],
    write(S-G), nl,
    arrangements(S, G, M), write(M).
    */
    /*
    maplist(arrangements, List, A),
    sum(A, #=, Ans1),
    write_part1(Ans1).
    */

simplify([], [], []).
simplify(List, [], [List-L]) :-
    List = [_|_],
    length(List, L).
simplify([], ['.'|T], Out) :-
    simplify([], T, Out).
simplify(List, ['.'|T], [List-L|Out]) :-
    List = [_|_],
    length(List, L),
    simplify([], T, Out).
simplify(List, ['#'|T], Out) :-
    append(List, ['#'], NList),
    simplify(NList, T, Out).
simplify(List, ['?'|T], Out) :-
    append(List, ['?'], NList),
    simplify(NList, T, Out).

arrangements([], [], 1).
arrangements([], [_|_], 0).
arrangements(List, Groups, N) :- mem(List, Groups, N), !.
arrangements([H-LH|T], Groups, N) :-
    findall(Z, (
        append(A, B, Groups),
        % check H can ever cover A in order to filter
        sum(A, #=, Sum),
        length(A, LA),
        LH #>= Sum + LA - 1,
        ( A = [AA|_], LH #< AA -> Z #= 0 ;
        arrangements(H-A, X),
        arrangements(T, B, Y),
        Z #= X*Y )
    ), Ways),
    sum(Ways, #=, N),
    assertz(mem([H-LH|T], Groups, N)).

arrangements(Springs-Groups, N) :-
    arrangement(Springs, Groups, N).

arrangement(S, G, N) :- mem(S, G, N), !.

arrangement([], [], 1).
arrangement([], [_|_], 0).
arrangement([C|T], Groups, N) :-
    length(T, TLen),
    arrangement([C|T], TLen, Groups, N).

arrangement(['#'|_], _, [], 0).
arrangement(['#'|T], TLen, [X|Groups], 0) :-
    Y #= X-1,
    TLen #< Y,
    assertz(mem(['#'|T], [X|Groups], 0)).
arrangement(['#'|T], TLen, [X|Groups], N) :-
    Y #= X-1,
    TLen #= Y,
    ( Groups == [] -> N#=1; N#=0 ),
    assertz(mem(['#'|T], [X|Groups], N)).
arrangement(['#'|T], TLen, [X|Groups], N) :-
    Y #= X-1,
    TLen #> Y,
    length(Pref, Y),
    append(Pref, [H|Rem], T),
    ( H == '?' -> arrangement(Rem, Groups, N) ; N = 0 ),
    assertz(mem(['#'|T], [X|Groups], N)).
arrangement(['?'|T], _, [], N) :-
    arrangement(T, [], N).
arrangement(['?'|T], TLen, [X|Groups], 0) :-
    Y #= X-1,
    TLen #< Y,
    assertz(mem(['?'|T], [X|Groups], 0)).
arrangement(['?'|T], TLen, [X|Groups], N) :-
    Y #= X-1,
    TLen #= Y,
    ( Groups == [] -> N#=1; N#=0 ),
    assertz(mem(['?'|T], [X|Groups], N)).
arrangement(['?'|T], TLen, [X|Groups], N) :-
    Y #= X-1,
    TLen #> Y,
    arrangement(T, [X|Groups], A), % '?' -> '.'
    length(Pref, Y),
    append(Pref, [H|Rem], T),
    ( H == '?' -> arrangement(Rem, Groups, M), N #= A + M ; N = A ),
    assertz(mem(['?'|T], [X|Groups], N)).

/*
arrangements(Springs-Groups, N) :-
    ( mem(Springs, Groups, X) -> N = X;
        bagof(_, arrangement(Springs, Groups), List), length(List, N),
        assertz(mem(Springs, Groups, N))
    ).
    %findall(_, arrangement(Springs, Groups), List),
    %length(List, N).

arrangement([], []).
arrangement(['#'|T], [N|Groups]) :-
    NN #= N-1,
    length(Pref, NN),
    append(Pref, ['?'|Rem], T),
    arrangement(Rem, Groups).
arrangement(['#'|T], [N]) :-
    NN #= N-1,
    length(T, NN).
arrangement(['?'|T], Groups) :-
    arrangement(['#'|T], Groups).
arrangement(['?'|T], Groups) :-
    arrangement(T, Groups).
*/

/*
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
    */

quintuple(List, Quints) :-
    append(List, List, Twice),
    append(Twice, Twice, Four),
    append(List, Four, Quints).

quintuple_join(List, Quints) :-
    append(List, ['?'|List], Twice),
    append(Twice, ['?'|Twice], Four),
    append(List, ['?'|Four], Quints).

parse([S-N|T]) --> springs(S), intlist(N), parse(T).
parse([]) --> [].

springs(['.'|T]) --> ".", springs(T).
springs(['#'|T]) --> "#", springs(T).
springs(['?'|T]) --> "?", springs(T).
springs([]) --> " ".

intlist([N|T]) --> integer(N), ",", intlist(T).
intlist([N]) --> integer(N), "\n".

%integer(N) --> digits(D), {write(D), number_codes(N, D)}.
%digits([H|T]) --> [H], {H\==','}, digits(T).
%digits([X]) --> [X], {X\==','}.
