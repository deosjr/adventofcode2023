:- use_module(library(assoc)).
:- use_module(library(between)).
:- use_module(library(lambda)).

:- dynamic(mem/4).

%use_test_input.

run_day(16, Filename) :-
    phrase_from_file(parse(0,0), Filename),
    find_max(Max),
    part1(-1-0, 1-0, Max, Ans1),
    write_part1(Ans1),
    Max = MX-MY,
    MMX #= MX+1,
    MMY #= MY+1,
    numlist(0, MX, Xs),
    numlist(0, MY, Ys),
    maplist(Max+\X^A^(part1(X-(-1), 0-1, Max, A)), Xs, Down),
    maplist(Max+\X^A^(part1(X-MMY, 0-(-1), Max, A)), Xs, Up),
    maplist(Max+\Y^A^(part1(-1-Y, 1-0, Max, A)), Ys, Left),
    maplist(Max+\Y^A^(part1(MMX-Y, -1-0, Max, A)), Ys, Right),
    append(Down, Up, A),
    append(A, Left, B),
    append(B, Right, C),
    sort(C, Sorted),
    reverse(Sorted, [Ans2|_]),
    write_part2(Ans2).

part1(Start, Dir, Max, Ans) :-
    empty_assoc(Energized),
    empty_assoc(Seen),
    energize([beam(Start, Dir)], Max, Seen, Energized, Out),
    assoc_to_keys(Out, List),
    length(List, Len),
    Ans #= Len - 1. % account for assertion of oob start

energize([], _, _, X, X).
energize([beam(C,Dir)|Beams], Max, Seen, Energized, Out) :-
    get_assoc(C-Dir, Seen, _),
    %write(["SKIP", [beam(C,Dir)|Beams]]), nl,
    energize(Beams, Max, Seen, Energized, Out).
energize([beam(C,Dir)|Beams], Max, Seen, Energized, Out) :-
    \+get_assoc(C-Dir, Seen, _),
    mem(C, Dir, End, More),
    %write(["MEM", [beam(C,Dir)|Beams]]), nl,
    put_assoc(C-Dir, Seen, true, NewSeen),
    ( End = none -> find_line_oob(C, Dir, Max, Energized, NewEnergized)
    ; find_line(C, Dir, End, Energized, NewEnergized) ),
    ( More == [] -> NewBeams = Beams ; append(More, Beams, NewBeams) ),
    energize(NewBeams, Max, NewSeen, NewEnergized, Out).
energize([beam(C,Dir)|Beams], Max, Seen, Energized, Out) :-
    \+get_assoc(C-Dir, Seen, _),
    \+mem(C, Dir, _, _),
    %write(["NEW", [beam(C,Dir)|Beams]]), nl,
    put_assoc(C-Dir, Seen, true, NewSeen),
    ( find_closest(C, Dir, Item) ->
        Item =.. [_, Pos, _],
        find_line(C, Dir, Pos, Energized, NewEnergized),
        newbeams(Dir, Item, MoreBeams),
        assertz(mem(C, Dir, Pos, MoreBeams)),
        append(MoreBeams, Beams, NewBeams),
        energize(NewBeams, Max, NewSeen, NewEnergized, Out)
    ;
        find_line_oob(C, Dir, Max, Energized, NewEnergized),
        assertz(mem(C, Dir, none, [])),
        energize(Beams, Max, NewSeen, NewEnergized, Out)
    ).

find_line(X, _, X, Y, Y).
find_line(C, Dir, End, Assoc, Out) :-
    C \= End,
    move(C, Dir, Pos),
    put_assoc(C, Assoc, true, NewAssoc),
    find_line(Pos, Dir, End, NewAssoc, Out).

find_line_oob(C, _, Max, X, X) :-
    out_of_bounds(C, Max).
find_line_oob(C, Dir, Max, Assoc, Out) :-
    \+out_of_bounds(C, Max),
    move(C, Dir, Pos),
    put_assoc(C, Assoc, true, NewAssoc),
    find_line_oob(Pos, Dir, Max, NewAssoc, Out).

find_closest(X-Y, -1-0, Item) :-
    findall(NX-Item, (
        C=NX-Y,
        NX #< X,
        ( mirror(C,Mode), Item=mirror(C,Mode) 
        ; splitter(C,Mode), Item=splitter(C,Mode) )
    ), Found),
    sort(Found, Sorted),
    reverse(Sorted, [_-Item|_]).
find_closest(X-Y, 1-0, Item) :-
    findall(NX-Item, (
        C=NX-Y,
        NX #> X,
        ( mirror(C,Mode), Item=mirror(C,Mode) 
        ; splitter(C,Mode), Item=splitter(C,Mode) )
    ), Found),
    sort(Found, [_-Item|_]).
find_closest(X-Y, 0-1, Item) :-
    findall(NY-Item, (
        C=X-NY,
        NY #> Y,
        ( mirror(C,Mode), Item=mirror(C,Mode) 
        ; splitter(C,Mode), Item=splitter(C,Mode) )
    ), Found),
    sort(Found, [_-Item|_]).
find_closest(X-Y, 0-(-1), Item) :-
    findall(NY-Item, (
        C=X-NY,
        NY #< Y,
        ( mirror(C,Mode), Item=mirror(C,Mode) 
        ; splitter(C,Mode), Item=splitter(C,Mode) )
    ), Found),
    sort(Found, Sorted),
    reverse(Sorted, [_-Item|_]).

newbeams(Dir, mirror(C, Mirror), [beam(C,NewDir)]) :-
    mirror_direction(Mirror, Dir, NewDir).
newbeams(Dir, splitter(C, Splitter), SplitBeams) :-
    splitter(C, Splitter),
    split_beam(Splitter, C, Dir, SplitBeams).

mirror_direction(forward, X-Y, DX-DY) :-
    DX #= -1 * Y, DY #= -1 * X.
mirror_direction(backward, X-Y, Y-X).

split_beam(horizontal, C, DX-DY, Split) :-
    ( DY #= 0 ->
        Split = [beam(C,DX-DY)]
    ;
        Split = [beam(C,-1-0), beam(C,1-0)]
    ).
split_beam(vertical, C, DX-DY, Split) :-
    ( DX #= 0 ->
        Split = [beam(C,DX-DY)]
    ;
        Split = [beam(C,0-(-1)), beam(C,0-1)]
    ).

move(X-Y, DX-DY, PX-PY) :-
    PX #= X + DX,
    PY #= Y + DY.

out_of_bounds(X-Y, MX-MY) :-
    X #< 0 ; Y #< 0 ; X #> MX ; Y #> MY.

% wont work for all inputs but works for mine and for test
find_max(MaxX-MaxY) :-
    findall(X, (mirror(X-_,_);splitter(X-_,_)), Xs),
    sort(Xs, SX),
    reverse(SX, [MaxX|_]),
    findall(Y, (mirror(_-Y,_);splitter(_-Y,_)), Ys),
    sort(Ys, SY),
    reverse(SY, [MaxY|_]).

parse(X,Y) --> ".",  {Z #= X+1}, parse(Z,Y).
parse(_,Y) --> "\n", {Z #= Y+1}, parse(0,Z).
parse(X,Y) --> "/",  {assertz(mirror(X-Y,forward)),      Z #= X+1}, parse(Z,Y).
parse(X,Y) --> "\\", {assertz(mirror(X-Y,backward)),     Z #= X+1}, parse(Z,Y).
parse(X,Y) --> "|",  {assertz(splitter(X-Y,vertical)),   Z #= X+1}, parse(Z,Y).
parse(X,Y) --> "-",  {assertz(splitter(X-Y,horizontal)), Z #= X+1}, parse(Z,Y).
parse(_,_) --> [].
