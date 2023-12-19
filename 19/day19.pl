:- use_module(library(charsio)).

run_day(19, Filename) :-
    phrase_from_file(parse(Flows, Parts), Filename),
    maplist(accept(Flows, in), Parts, Out),
    sum(Out, #=, Ans1),
    write_part1(Ans1).

accept(_, rejected, _, 0).
accept(_, accepted, Part, Out) :-
    rating(Part, Out).
accept(Flows, Flow, Part, Out) :-
    Flow \= accepted, Flow \= rejected,
    memberchk(workflow(Flow, Rules), Flows),
    apply_rules(Rules, Part, NewFlow),
    accept(Flows, NewFlow, Part, Out).

apply_rules([rule(X)], _, X).
apply_rules([rule(D,C,N,L),B|T], Part, New) :-
    ( cmp(Part, D, C, N) -> New = L ;
      apply_rules([B|T], Part, New) ).

rating(part(X,M,A,S), R) :- R #= X+M+A+S.

cmp(part(X,_,_,_), x, >, N) :- X #> N.
cmp(part(X,_,_,_), x, <, N) :- X #< N.
cmp(part(_,M,_,_), m, >, N) :- M #> N.
cmp(part(_,M,_,_), m, <, N) :- M #< N.
cmp(part(_,_,A,_), a, >, N) :- A #> N.
cmp(part(_,_,A,_), a, <, N) :- A #< N.
cmp(part(_,_,_,S), s, >, N) :- S #> N.
cmp(part(_,_,_,S), s, <, N) :- S #< N.

parse(Flows, Parts) --> parse_workflows(Flows), parse_parts(Parts).

parse_workflows([workflow(Name, Rules)|T]) -->
    label(Name), "{", parse_rules(Rules), "}\n", parse_workflows(T).
parse_workflows([]) --> "\n".

parse_rules([rule(D, C, N, L)|T]) -->
    parse_dim(D), parse_cmp(C), integer(N), ":", label(L), ",", parse_rules(T).
parse_rules([rule(L)]) --> label(L).

parse_parts([part(X,M,A,S)|T]) -->
    "{x=", integer(X), ",m=", integer(M), ",a=", integer(A), ",s=", integer(S), "}\n", parse_parts(T).
parse_parts([]) --> [].

parse_dim(x) --> "x".
parse_dim(m) --> "m".
parse_dim(a) --> "a".
parse_dim(s) --> "s".

parse_cmp(=) --> "=".
parse_cmp(>) --> ">".
parse_cmp(<) --> "<".

integer(N) --> digits(D), {number_chars(N, D)}.
digits([H|T]) --> [H], {char_type(H, decimal_digit)}, digits(T).
digits([X]) --> [X], {char_type(X, decimal_digit)}.

label(Label) --> alphas(A), {A=="A" -> Label=accepted ; ( A="R" -> Label=rejected ; atom_chars(Label, A))}.
alphas([H|T]) --> [H], {char_type(H, alphabetic)}, alphas(T).
alphas([X]) --> [X], {char_type(X, alphabetic)}.
