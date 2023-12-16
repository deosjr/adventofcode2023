:- dynamic(energized/2).

run_day(16, Filename) :-
    phrase_from_file(parse(0,0), Filename),
    find_max(Max),
    energize([beam(0-0,1-0)], Max),
    findall(C, energized(C,_), List),
    list_to_set(List, Set),
    length(Set, Ans1),
    write_part1(Ans1).

energize([], _).
energize([beam(C,Dir)|Beams], Max) :-
    ( energized(C,Dir) ; out_of_bounds(C,Max) ),
    energize(Beams, Max).
energize([beam(C,Dir)|Beams], Max) :-
    \+energized(C, Dir),
    \+out_of_bounds(C, Max),
    assertz(energized(C, Dir)),
    newbeams(C, Dir, MoreBeams),
    append(MoreBeams, Beams, NewBeams),
    energize(NewBeams, Max).

newbeams(C, Dir, [beam(NewPos,NewDir)]) :-
    mirror(C, Mirror),
    mirror_direction(Mirror, Dir, NewDir),
    move(C, NewDir, NewPos).
newbeams(C, Dir, SplitBeams) :-
    splitter(C, Splitter),
    split_beam(Splitter, C, Dir, SplitBeams).
newbeams(C, Dir, [beam(NewPos,Dir)]) :-
    \+mirror(C,_), \+splitter(C,_),
    move(C, Dir, NewPos).

mirror_direction(forward, X-Y, DX-DY) :-
    DX #= -1 * Y, DY #= -1 * X.
mirror_direction(backward, X-Y, Y-X).

split_beam(horizontal, C, DX-DY, Split) :-
    ( DY #= 0 ->
        move(C, DX-DY, NewPos),
        Split = [beam(NewPos,DX-DY)]
    ;
        move(C, -1-0, Left),
        move(C, 1-0, Right),
        Split = [beam(Left,-1-0), beam(Right,1-0)]
    ).
split_beam(vertical, C, DX-DY, Split) :-
    ( DX #= 0 ->
        move(C, DX-DY, NewPos),
        Split = [beam(NewPos,DX-DY)]
    ;
        move(C, 0-(-1), Up),
        move(C, 0-1, Down),
        Split = [beam(Up,0-(-1)), beam(Down,0-1)]
    ).

move(X-Y, DX-DY, PX-PY) :-
    PX #= X + DX,
    PY #= Y + DY.

%left(X-Y, Y-DY)  :- DY #= -1 * X.
%right(X-Y, DX-X) :- DX #= -1 * Y.

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
