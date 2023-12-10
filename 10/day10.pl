:- dynamic(filled/1).

run_day(10, Filename) :-
    phrase_from_file(parse(0,0), Filename),
    loop(Ans1),
    write_part1(Ans1),
    %pipe(C, none, east, none, west), % -
    %part_of_loop(C), C=_-0,
    %floodfill_inner(C, east, south),
    pipe(C, south, none, north, none), % |
    part_of_loop(C),
    can_reach_edge(C, west), % cant reach edge east is implied
    % now we know that at C, east is inside of loop
    % walk the loop, floodfilling inside
    % TODO: why is there a difference in walking direction around the loop?
    % why do I have to walk both ways in order to find all?!?
    floodfill_inner(C, north, east),
    floodfill_inner(C, south, east),
    findall(Coord, filled(Coord), Inner),
    length(Inner, Ans2),
    write_part2(Ans2).

% etc for north, east, south
can_reach_edge(0-_, west).
can_reach_edge(C, west) :-
    C = X-_, X #> 0,
    step(C, To, west),
    \+(part_of_loop(To)),
    can_reach_edge(To, west).

loop(Ans) :-
    find_start(Start, InitialDir),
    loop(Start, InitialDir),
    findall(Node, part_of_loop(Node), Loop),
    length(Loop, L),
    Ans #= L//2.

loop(Start, Dir) :-
    step(Start, C, Dir),
    assertz(part_of_loop(C)),
    follow_pipe(C, Dir, NewDir),
    loop(Start, C, NewDir).

loop(Start, C, Dir) :-
    step(C, NewC, Dir),
    assertz(part_of_loop(NewC)),
    ( Start == NewC ->
        true
        ;
        follow_pipe(NewC, Dir, NewDir),
        loop(Start, NewC, NewDir)
    ).

% TODO: essentially do once + loop
floodfill_inner(Node, WalkDir, InsideDir) :-
    step(Node, In, InsideDir),
    floodfill([In]),
    step(Node, To, WalkDir),
    follow_pipe(To, WalkDir, NewWalkDir),
    change_inside_dir(WalkDir, InsideDir, NewWalkDir, NewInsideDir),
    floodfill_inner(To, Node, NewWalkDir, NewInsideDir).

floodfill_inner(End, End, _, _).
floodfill_inner(Node, End, WalkDir, InsideDir) :-
    Node \== End,
    step(Node, In, InsideDir),
    floodfill([In]),
    step(Node, To, WalkDir),
    follow_pipe(To, WalkDir, NewWalkDir),
    change_inside_dir(WalkDir, InsideDir, NewWalkDir, NewInsideDir),
    floodfill_inner(To, End, NewWalkDir, NewInsideDir).

change_inside_dir(WalkDir, InsideDir, WalkDir, InsideDir).
% 16 options from here: walk (4 options) has two orthogonal directions for inside
% newwalk is 2 options, because we never about-face and if we continue thats the above case
% 4x2x2 = 16
change_inside_dir(north, west, west, south).
change_inside_dir(north, west, east, north).
change_inside_dir(north, east, west, north).
change_inside_dir(north, east, east, south).

change_inside_dir(west, north, north, east).
change_inside_dir(west, north, south, west).
change_inside_dir(west, south, north, west).
change_inside_dir(west, south, south, east).

change_inside_dir(south, west, west, north).
change_inside_dir(south, west, east, south).
change_inside_dir(south, east, west, south).
change_inside_dir(south, east, east, north).

change_inside_dir(east, north, north, west).
change_inside_dir(east, north, south, east).
change_inside_dir(east, south, north, east).
change_inside_dir(east, south, south, west).

floodfill([]).
floodfill([C|T]) :-
    ( (filled(C);part_of_loop(C)) ->
        floodfill(T)
    ;
        assertz(filled(C)),
        step(C, N, north),
        step(C, W, west),
        step(C, S, south),
        step(C, E, east),
        floodfill([N,W,S,E|T])
    ).

step(X-Y, X-DY, north) :- DY #= Y-1.
step(X-Y, DX-Y, west)  :- DX #= X-1.
step(X-Y, X-DY, south) :- DY #= Y+1.
step(X-Y, DX-Y, east)  :- DX #= X+1.

% we step into C with Dir From, and follow the pipe outwards towards To
follow_pipe(C, north, To) :-
    pipe(C, _, _, To, _).
follow_pipe(C, west, To) :-
    pipe(C, _, _, _, To).
follow_pipe(C, south, To) :-
    pipe(C, To, _, _, _).
follow_pipe(C, east, To) :-
    pipe(C, _, To, _, _).

find_pipe(C, Dir, Out) :-
    ( follow_pipe(C, Dir, To) -> Out=To ; Out=none ).

% start is guaranteed to have exactly 2 connected pipes, return one of them
find_start(X-Y, Dir) :-
    start(X, Y),
    step(X-Y, N, north),
    step(X-Y, W, west),
    step(X-Y, S, south),
    step(X-Y, E, east),
    find_pipe(N, north, NN),
    find_pipe(W, west, WW),
    find_pipe(S, south, SS),
    find_pipe(E, east, EE),
    ( WW==none,EE==none -> assertz(pipe(X-Y, south, none, north, none)), Dir=south; true), % |
    ( NN==none,SS==none -> assertz(pipe(X-Y, none, east, none, west)), Dir=east;    true), % -
    ( WW==none,SS==none -> assertz(pipe(X-Y, east, none, none, north)), Dir=east;   true), % L
    ( SS==none,EE==none -> assertz(pipe(X-Y, west, north, none, none)), Dir=west;   true), % J
    ( NN==none,EE==none -> assertz(pipe(X-Y, none, south, west, none)), Dir=south;  true), % 7
    ( NN==none,WW==none -> assertz(pipe(X-Y, none, none, east, south)), Dir=east;   true). % F

find_first_connected(Start, [H|T], C, Dir) :-
    step(Start, To, H),
    ( follow_pipe(To, H, D), D \==none -> C=To, Dir=D ; find_first_connected(Start, T, C, Dir) ).

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
