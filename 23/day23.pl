:- use_module(library(lambda)).

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
    retract(path(End)),
    assertz(start(Start)),
    assertz(end(End)),
    % find all slopes, for each do:
    % walk paths, never walk back. should be a unique path forward
    % if you hit another slope, assert pathlength between the two as a fact
    % now you have a directed graph you can do easier search in
    % if the graph is acyclic (is it?), this is shortest path on negative weights
    %% part 2
    assertz(vertex(Start)),
    assertz(vertex(End)),

    % for part 2, the vertices are crossroads instead of slopes
    findall(C, (
        path(C),
        dirs(Dirs),
        maplist(C+\X^Y^(dir(X, D), add(C, D, Y)), Dirs, Neighbours),
        maplist(\X^Y^((slope(X,_) -> Y#=1;Y#=0)), Neighbours, Counts),
        sum(Counts, #>, 2),
        assertz(vertex(C))
    ), _),

    findall(C, (vertex(C), walk(C)), _),

    %% pruning: some edges are impossible
    retractall(edge(_, Start, _)),
    retractall(edge(End, _, _)),
    findall(V, (edge(Start, X, _), edge(X, V, _), retract(edge(V, X, _))), _),
    findall(V, (edge(X, End, _), edge(V, X, _), retract(edge(X, V, _))), _),

    listing(edge/3),
    %% conjecture: longest path visits _all_ vertices (ie TSP)
    %% falsified after 3 hours of calculation :D

    %findall(N, hike(Start, End, [], 0, N), Ns),
    findall(N, (hike(N), N #> 6000, write(N),nl), Ns),
    list_max(Ns, Ans1),
    write_part1(Ans1).

walk(C) :-
    dirs(Dirs),
    maplist(C+\X^(dir(X, D), add(C, D, P), ((path(P);slope(P,_)) -> (walk(C, X)->true;true) ; true)), Dirs).

walk(C, Dir) :-
    walk(C, C, Dir, 0).

walk(From, C, Dir, N) :-
    dir(Dir, D),
    add(C, D, P),
    (path(P);slope(P,_)),
    NN #= N+1,
    findall(X, (dirs(Dirs), member(X,Dirs), \+opposite_dir(X, Dir)), NewDirs),
    maplist(explore(From, P, NN), NewDirs, Out),
    ( member(NewDir, Out), NewDir\=none ->
        walk(From, P, NewDir, NN) ; true ).

explore(From, C, N, Dir, NewDir) :-
    dir(Dir, D),
    add(C, D, P),
    ( vertex(P) -> NN #= N+1, assertz(edge(From, P, NN)) ; true ),
    ( (slope(P, _), N #> 1) -> NN #= N+2, add(P,D,PP), assertz(edge(From, PP, NN)) ; true ),
    ( path(P) -> NewDir = Dir ; NewDir = none ).

hike(N) :-
    findall(V, (vertex(V), \+start(V), \+end(V)), Vs),
    start(Start),
    hike(Start, Vs, 0, N).

hike(V, [_], X, Y) :-
    end(End),
    edge(V, End, N),
    Y #= X + N.
hike(From, Vs, Acc, Sum) :-
    select(To, Vs, Rem),
    Rem = [_|_],
    edge(From, To, N),
    NAcc #= Acc + N,
    hike(To, Rem, NAcc, Sum).

/*
hike(End, End, _, X, X).
hike(From, End, List, Acc, Sum) :-
    From \= End,
    edge(From, To, N),
    \+member(To, List),
    NAcc #= Acc + N,
    hike(To, End, [From|List], NAcc, Sum).
    */

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
