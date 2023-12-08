:- use_module(library(charsio)).

run_day(8, Filename) :-
    phrase_from_file(parse(Instructions), Filename),
    track(Instructions, Instructions, "AAA", done_p1, 0, Ans1),
    write_part1(Ans1),
    findall(X, (node(X,_,_), start(X)), StartingNodes),
    maplist(part2(Instructions), StartingNodes, _Paths),
    %write(Paths),
    % assume all instantly cyclical, no other Z-ending paths in between
    %lowest_common_multiple(Paths, Ans2), TODO
    % throw into WolframAlpha instead, get output
    write_part2(14299763833181).

track([], Ins, Node, Func, N, Out) :-
    track(Ins, Ins, Node, Func, N, Out).

track([Dir|T], Ins, Path, Func, N, Out) :-
    step(Dir, Path, To),
    M #= N + 1,
    ( call(Func,To) -> Out #= M ; track(T, Ins, To, Func, M, Out) ).

part2(Instructions, Node, Out) :-
    track(Instructions, Instructions, Node, done_p2, 0, Out).

% something is still weird with quotes, X=[_,_,'A'] doesnt work
done_p1("ZZZ").
done_p2([_,_|S]) :- S="Z".
start([_,_|S]) :- S="A".

step('L', From, To) :- node(From, To, _).
step('R', From, To) :- node(From, _, To).

%lowest_common_multiple()

parse(Instructions) --> letters(Instructions), "\n\n", parse.
parse --> letters(From), " = (", letters(Left), ", ", letters(Right), ")\n", {assertz(node(From,Left,Right))}, parse.
parse --> [].

letters([H|T]) --> [H], {char_type(H, alphanumeric)}, letters(T).
letters([]) --> [].
