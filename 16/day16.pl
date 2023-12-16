:- use_module(library(between)).
:- use_module(library(lambda)).

:- dynamic(energized/2).

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
    retractall(energized(_,_)),
    energize([beam(Start, Dir)], Max),
    findall(C, energized(C,_), List),
    list_to_set(List, Set),
    length(Set, Len),
    Ans #= Len - 1. % account for assertion of oob start

energize([], _).
energize([beam(C,Dir)|Beams], Max) :-
    energized(C,Dir),
    energize(Beams, Max).
energize([beam(C,Dir)|Beams], Max) :-
    \+energized(C, Dir),
    %write([beam(C,Dir)|Beams]), nl,
    ( find_closest(C, Dir, Item) ->
        Item =.. [_, Pos, _],
        %write(Item), nl,
        assert_line(C, Dir, Pos),
        newbeams(Dir, Item, MoreBeams),
        append(MoreBeams, Beams, NewBeams),
        energize(NewBeams, Max)
    ;
        assert_until_oob(C, Dir, Max),
        energize(Beams, Max)
    ).

assert_line(X, _, X).
assert_line(C, Dir, End) :-
    C \= End,
    assertz(energized(C, Dir)),
    move(C, Dir, Pos),
    assert_line(Pos, Dir, End).

assert_until_oob(C, Dir, Max) :-
    ( out_of_bounds(C, Max) -> true ;
        assertz(energized(C,Dir)), move(C,Dir,Pos), assert_until_oob(Pos,Dir,Max) ).

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
