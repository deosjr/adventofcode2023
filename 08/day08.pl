:- use_module(library(charsio)).

run_day(8, Filename) :-
    phrase_from_file(parse(Instructions), Filename),
    part1(Instructions, Instructions, "AAA", "ZZZ", 0, Ans1),
    write_part1(Ans1).

part1([], Ins, Source, Dest, Path, Out) :-
    part1(Ins, Ins, Source, Dest, Path, Out).

part1(['L'|T], Ins, Source, Dest, Path, Out) :-
    node(Source, Left, _),
    N #= Path + 1,
    ( Left = Dest -> Out #= N ; part1(T, Ins, Left, Dest, N, Out) ).

part1(['R'|T], Ins, Source, Dest, Path, Out) :-
    node(Source, _, Right),
    N #= Path + 1,
    ( Right = Dest -> Out #= N ; part1(T, Ins, Right, Dest, N, Out) ).

parse(Instructions) --> letters(Instructions), "\n\n", parse.
parse --> letters(From), " = (", letters(Left), ", ", letters(Right), ")\n", {assertz(node(From,Left,Right))}, parse.
parse --> [].

letters([H|T]) --> [H], {char_type(H, alpha)}, letters(T).
letters([]) --> [].
