:- use_module(library(charsio)).

:- dynamic(box/2).

run_day(15, Filename) :-
    phrase_from_file(parse(List), Filename),
    maplist(hash, List, Hashes),
    sum(Hashes, #=, Ans1),
    write_part1(Ans1),
    phrase_from_file(parse2(Instr), Filename),
    maplist(instr, Instr),
    findall(N-L, box(N, L), Boxes),
    maplist(focus, Boxes, Ns),
    sum(Ns, #=, Ans2),
    write_part2(Ans2).

instr(add(Label, Hash, N)) :-
    ( box(Hash, List) -> retract(box(Hash, List)) ; List = [] ),
    insert(List, Label, N, New),
    assertz(box(Hash, New)).

instr(remove(Label, Hash)) :-
    ( box(Hash, List) -> true ; List = [] ),
    ( select(lens(Label,_), List, New) ->
        retract(box(Hash, List)), assertz(box(Hash, New)) ; true ).

insert(List, Label, N, New) :-
    ( nth0(I, List, lens(Label,_), R) ->
        nth0(I, New, lens(Label,N), R)
    ;
        append(List, [lens(Label,N)], New)
    ).

hash(List, Hash) :- hash(List, 0, Hash).

hash([], X, X).
hash([H|T], Acc, N) :-
    char_code(H, C),
    NAcc #= (17 * (Acc + C)) mod 256,
    hash(T, NAcc, N).

focus(N-L, Ans) :-
    NN #= (N+1),
    focus(L, NN, 1, 0, Ans).

focus([], _, _, X, X).
focus([lens(_, N)|T], BoxID, Idx, Acc, Out) :-
    NAcc #= BoxID * Idx * N + Acc,
    NIdx #= Idx + 1,
    focus(T, BoxID, NIdx, NAcc, Out).

parse([H|T]) --> parse_step(H), ",", parse(T).
parse([H]) --> parse_step(H), "\n".

% because C \== ',' breaks...
parse_step([C|T]) --> [C], { [C] \== "," }, parse_step(T).
parse_step([]) --> [].

parse2([H|T]) --> parse_instr(H), ",", parse2(T).
parse2([H]) --> parse_instr(H), "\n".

parse_instr(add(A, H, N)) --> label(L), "=", [D],
    {atom_chars(A, L), hash(L, H), number_chars(N, [D])}.
parse_instr(remove(A, H)) --> label(L), "-",
    {atom_chars(A, L), hash(L, H)}.

label([H|T]) --> [H], {char_type(H, alphabetic)}, label(T).
label([X]) --> [X], {char_type(X, alphabetic)}.
