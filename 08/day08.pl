:- use_module(library(charsio)).

%use_test_input.

run_day(8, Filename) :-
    phrase_from_file(parse(Instructions), Filename),
    part1(Instructions, Instructions, "AAA", "ZZZ", 0, Ans1),
    write_part1(Ans1),
    % something is still weird with quotes, X=[_,_,'A'] doesnt work
    findall(X, (node(X,_,_), X=[_,_|S], S="A"), StartingNodes),
    part2(Instructions, Instructions, StartingNodes, 0, Ans2),
    write_part2(Ans2).

step('L', From, To) :- node(From, To, _).
step('R', From, To) :- node(From, _, To).

part1([], Ins, Source, Dest, Path, Out) :-
    part1(Ins, Ins, Source, Dest, Path, Out).

part1([Dir|T], Ins, Source, Dest, Path, Out) :-
    step(Dir, Source, To),
    N #= Path + 1,
    ( To = Dest -> Out #= N ; part1(T, Ins, To, Dest, N, Out) ).

part2([], Ins, Nodes, Path, Out) :-
    part2(Ins, Ins, Nodes, Path, Out).

part2([Dir|T], Ins, Nodes, Path, Out) :-
    maplist(step(Dir), Nodes, NewNodes),
    N #= Path + 1,
    ( done(NewNodes) -> Out #= N ; part2(T, Ins, NewNodes, N, Out) ).

done([]).
done([H|T]) :-
    H = [_,_|S], S="Z",
    done(T).

parse(Instructions) --> letters(Instructions), "\n\n", parse.
parse --> letters(From), " = (", letters(Left), ", ", letters(Right), ")\n", {assertz(node(From,Left,Right))}, parse.
parse --> [].

letters([H|T]) --> [H], {char_type(H, alphanumeric)}, letters(T).
letters([]) --> [].
