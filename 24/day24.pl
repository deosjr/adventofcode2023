:- use_module(library(charsio)).
:- use_module(library(lambda)).

%use_test_input.

run_day(24, Filename) :-
    phrase_from_file(parse(List), Filename),
    intersections(List),
    findall(P-Q, found(P,Q), Out),
    length(Out, Ans1),
    write_part1(Ans1).

intersections([]).
intersections([H|T]) :-
    intersections(H, T),
    intersections(T).

intersections(_, []).
intersections(P, [Q|T]) :-
    P = (X1-Y1-_)/(VX1-VY1-_),
    Q = (X2-Y2-_)/(VX2-VY2-_),
    write(P),nl,
    write(Q),nl,
    A is VY1 / VX1,
    B is Y1 - X1*A,
    C is VY2 / VX2,
    D is Y2 - X2*C,
    (A = C -> write(parallel),nl, intersections(P,T) ;
    X is (D - B) / (A - C),
    ( past(X, P, Q) -> write(crossed_in_past),nl, intersections(P,T) ;
    Y is A*X + B,
    ( inside_area(X, Y) -> write(inside),nl, assertz(found(P,Q)) ; write(outside),nl ),
    write(X-Y),nl,
    intersections(P, T))).

past(X, P, Q) :-
    P = (X1-Y1-_)/(VX1-VY1-_),
    Q = (X2-Y2-_)/(VX2-VY2-_),
    ( VX1 < 0, X > X1 ;
      VX1 > 0, X < X1 ;
      VX2 < 0, X > X2 ;
      VX2 > 0, X < X2 ).

inside_area(X, Y) :-
    %Min = 7,
    %Max = 27,
    Min = 200000000000000,
    Max = 400000000000000,
    X >= Min,
    X =< Max,
    Y >= Min,
    Y =< Max.

parse([Pos/Vel|T]) --> coord(Pos), " @ ", coord(Vel), "\n", parse(T).
parse([]) --> [].

coord(X-Y-Z) --> integer(X), ", ", integer(Y), ", ", integer(Z).

integer(N) --> "-", digits(D), {number_chars(X, D), N #= -X}.
integer(N) --> digits(D), {number_chars(N, D)}.
digits([H|T]) --> [H], {char_type(H, decimal_digit)}, digits(T).
digits([X]) --> [X], {char_type(X, decimal_digit)}.
