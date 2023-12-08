:- use_module(library(charsio)).

run_day(8, Filename) :-
    phrase_from_file(parse(Instructions), Filename),
    track(Instructions, Instructions, "AAA", done_p1, 0, Ans1),
    write_part1(Ans1),
    findall(X, (node(X,_,_), start(X)), StartingNodes),
    maplist(part2(Instructions), StartingNodes, Paths),
    % assume all instantly cyclical, no other Z-ending paths in between
    foldl(lcm, Paths, 1, Ans2),
    write_part2(Ans2).

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

lcm(X,Y,Z) :- Abs #= abs(X*Y), gcd(X,Y,G), Z #= Abs // G.

gcd(X,X,X).
gcd(X,Y,G) :- X#<Y, Y1 #= Y-X, gcd(X,Y1,G).
gcd(X,Y,G) :- X#>Y, gcd(Y,X,G).

parse(Instructions) --> letters(Instructions), "\n\n", parse.
parse --> letters(From), " = (", letters(Left), ", ", letters(Right), ")\n", {assertz(node(From,Left,Right))}, parse.
parse --> [].

letters([H|T]) --> [H], {char_type(H, alphanumeric)}, letters(T).
letters([]) --> [].
