run_day(11, Filename) :-
    phrase_from_file(parse(0,0), Filename),
    findall(X, galaxy(X,_), XList),
    list_to_set(XList, XSet),
    sort(XSet, Xs),
    findall(Y, galaxy(_,Y), YList),
    list_to_set(YList, YSet),
    sort(YSet, Ys),
    findall(X-Y, galaxy(X,Y), Galaxies),
    expand_and_count(2, Galaxies, Xs, Ys, Ans1),
    write_part1(Ans1),
    expand_and_count(1000000, Galaxies, Xs, Ys, Ans2),
    write_part2(Ans2).

expand_and_count(Factor, Galaxies, Xs, Ys, Ans) :-
    expand(Factor, Galaxies, Xs, Ys, Expanded),
    shortest_paths(Expanded, 0, Ans).

expand(_, [], _, _, []).
expand(F, [X-Y|T], Xs, Ys, [NewX-NewY|TT]) :-
    expand_coord(F, X, Xs, X, NewX),
    expand_coord(F, Y, Ys, Y, NewY),
    expand(F, T, Xs, Ys, TT).

% we are guaranteed to find C in the list
expand_coord(_, X, [X|_], Y, Y).
expand_coord(Factor, C, [A,B|Cs], Acc, New) :-
    A #< C,
    N #= Acc + (Factor-1)*((B-A)-1),
    expand_coord(Factor, C, [B|Cs], N, New).

shortest_paths([], X, X).
shortest_paths([H|T], Acc, Ans) :-
    shortest_paths(T, Acc, NAcc),
    shortest_paths(H, T, N),
    Ans #= N + NAcc.
shortest_paths(_-_, [], 0).
shortest_paths(X-Y, [XX-YY|T], N) :-
    shortest_paths(X-Y, T, NN),
    N #= NN + abs(XX-X) + abs(YY-Y).

parse(X,Y) --> ".", {Z #= X+1}, parse(Z,Y).
parse(_,Y) --> "\n", {Z #= Y+1}, parse(0,Z).
parse(X,Y) --> "#", {assertz(galaxy(X,Y)), Z #= X+1}, parse(Z,Y).
parse(_,_) --> [].
