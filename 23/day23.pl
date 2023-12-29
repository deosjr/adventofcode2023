%use_test_input.

run_day(23, Filename) :-
    phrase_from_file(parse(0,0), Filename),
    %Max = 22,
    Max = 140,
    Start = _-0,
    path(Start),
    End = _-Max,
    path(End),
    retract(path(Start)),
    assertz(slope(Start, down)),
    retract(path(End)),
    assertz(end(End)),
    % find all slopes, for each do:
    % walk paths, never walk back. should be a unique path forward
    % if you hit another slope, assert pathlength between the two as a fact
    % now you have a directed graph you can do easier search in
    % if the graph is acyclic (is it?), this is shortest path on negative weights
    findall(C, (slope(C, Dir), walk(C, Dir)), _),
    findall(N, (hike(Start, End, H), sum(H, #=, N)), Ns),
    list_max(Ns, Ans1),
    write_part1(Ans1).

walk(C, Dir) :-
    walk(C, C, Dir, 0).

walk(From, C, Dir, N) :-
    dir(Dir, D),
    add(C, D, P),
    path(P),
    NN #= N+1,
    findall(X, (dirs(Dirs), member(X,Dirs), \+opposite_dir(X, Dir)), NewDirs),
    maplist(explore(From, P, NN), NewDirs, Out),
    ( member(NewDir, Out), NewDir\=none ->
        walk(From, P, NewDir, NN) ; true ).

explore(From, C, N, Dir, NewDir) :-
    dir(Dir, D),
    add(C, D, P),
    ( (slope(P, Dir) ; end(P)) -> NN #= N+1, assertz(edge(From, P, NN)) ; true ),
    ( path(P) -> NewDir = Dir ; NewDir = none ).

hike(End, End, []).
hike(From, End, [N|List]) :-
    edge(From, To, N),
    hike(To, End, List).

dir(up, 0-(-1)).
dir(down, 0-1).
dir(left, -1-0).
dir(right, 1-0).
dirs([up, down, left, right]).

opposite_dir(up, down).
opposite_dir(down, up).
opposite_dir(left, right).
opposite_dir(right, left).

add(X-Y, DX-DY, NX-NY) :-
    NX #= X+DX,
    NY #= Y+DY.

parse(_,Y) --> "\n", {Z #= Y+1}, parse(0,Z).
parse(X,Y) --> "#",  {Z #= X+1}, parse(Z,Y).
parse(X,Y) --> ".",  {assertz(path(X-Y)),         Z #= X+1}, parse(Z,Y).
parse(X,Y) --> "^",  {assertz(slope(X-Y, up)),    Z #= X+1}, parse(Z,Y).
parse(X,Y) --> "v",  {assertz(slope(X-Y, down)),  Z #= X+1}, parse(Z,Y).
parse(X,Y) --> "<",  {assertz(slope(X-Y, left)),  Z #= X+1}, parse(Z,Y).
parse(X,Y) --> ">",  {assertz(slope(X-Y, right)), Z #= X+1}, parse(Z,Y).
parse(_,_) --> [].
