run_day(10, Filename) :-
    phrase_from_file(parse(0,0), Filename),
    loop(Loop),
    length(Loop, L),
    Ans1 #= L//2,
    write_part1(Ans1).

loop([C|List]) :-
    start(X, Y),
    find_first_connected(X-Y, [north,west,south,east], C, Dir),
    loop(C, Dir, List).

loop(C, Dir, [NewC|T]) :-
    step(C, NewC, Dir),
    ( start(X,Y), X-Y == NewC ->
        T = []
        ;
        follow_pipe(NewC, Dir, NewDir),
        loop(NewC, NewDir, T)
    ).

step(X-Y, X-DY, north) :- DY #= Y-1.
step(X-Y, DX-Y, west)  :- DX #= X-1.
step(X-Y, X-DY, south) :- DY #= Y+1.
step(X-Y, DX-Y, east)  :- DX #= X+1.

% we step into C with Dir From, and follow the pipe outwards towards To
follow_pipe(C, north, To) :-
    pipe(C, _, _, To, _), To \== none.
follow_pipe(C, west, To) :-
    pipe(C, _, _, _, To), To \== none.
follow_pipe(C, south, To) :-
    pipe(C, To, _, _, _), To \== none.
follow_pipe(C, east, To) :-
    pipe(C, _, To, _, _), To \== none.

% start is guaranteed to have exactly 2 connected pipes, return one of them
find_first_connected(Start, [H|T], C, Dir) :-
    step(Start, To, H),
    ( follow_pipe(To, H, D) -> C=To, Dir=D ; find_first_connected(Start, T, C, Dir) ).

parse(X,Y) --> ".", {Z #= X+1}, parse(Z,Y).
parse(_,Y) --> "\n", {Z #= Y+1}, parse(0,Z).
parse(X,Y) --> parse_pipe(X,Y), {Z #= X+1}, parse(Z,Y).
parse(_,_) --> [].

% pipe(Coord, N, W, S, E), asserts where each side is connected to
parse_pipe(X,Y) --> "|", {assertz(pipe(X-Y, south, none, north, none))}.
parse_pipe(X,Y) --> "-", {assertz(pipe(X-Y, none, east, none, west))}.
parse_pipe(X,Y) --> "L", {assertz(pipe(X-Y, east, none, none, north))}.
parse_pipe(X,Y) --> "J", {assertz(pipe(X-Y, west, north, none, none))}.
parse_pipe(X,Y) --> "7", {assertz(pipe(X-Y, none, south, west, none))}.
parse_pipe(X,Y) --> "F", {assertz(pipe(X-Y, none, none, east, south))}.
parse_pipe(X,Y) --> "S", {assertz(start(X,Y))}.
