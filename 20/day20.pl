:- use_module(library(arithmetic)).
:- use_module(library(lambda)).
:- use_module(library(queues)).

run_day(20, Filename) :-
    phrase_from_file(parse, Filename),
    prep_conjuncts,
    press_button(1000, Lows, Highs),
    Ans1 #= Lows * Highs,
    write_part1(Ans1),
    %% this was used to generate a mermaid graph, output pasted into mermaid.live
    %broadcaster(B),
    %format("flowchart TD", []), nl,
    %format("B(broadcaster)", []), nl,
    %maplist(\X^(format("B --> ~w", [X]),nl), B),
    %findall(_, (flipflop(Name,To,_), mermaid('%', Name, To)), _),
    %findall(_, (conjunct(Name,To,_), mermaid('&', Name, To)), _).
    %
    %% from that graph we can see regularities
    %% series of flipflops representing a binary number
    %% that resets, encoding a cycle.
    %% rx first receives a low pulse when all subgraphs output low at the same time
    %% they take an extra button press to reset, so if X is binary number for subgraph,
    %% then X * N + (N-1) is button presses needed for that subgraph to output
    %% a low pulse the Nth time. This simplifies to X*(N+1) - 1, and equating all four
    %% subgraphs we can drop the -1. Now we are back at a LCM problem of 4 vars.
    foldl(lcm, [3821, 3761, 3889, 3943], 1, Ans2),
    write_part2(Ans2).

/*
mermaid(C, Name, To) :-
    ( C == '%' -> format("~w((~w))", [Name, Name]) ; format("~w{~w}", [Name, Name]) ), nl,
    maplist(Name+\X^(format("~w --> ~w", [Name,X]),nl), To).
*/

press_button(N, Lows, Highs) :-
    press_button(0, N, 0, Lows, 0, Highs).

press_button(X, X, Y, Y, Z, Z).
press_button(X, Out, AccL, Lows, AccH, Highs) :-
    X #< Out,
    button(L, H),
    NX #= X+1,
    NAccL #= AccL + L + 1, % button->broadcaster is an extra low
    NAccH #= AccH + H,
    %write([button, NX, NAccL, NAccH]), nl,
    press_button(NX, Out, NAccL, Lows, NAccH, Highs).

button(Lows, Highs) :-
    broadcast(Queue),
    handle_pulses(Queue, Lows, Highs).

broadcast(Q) :-
    broadcaster(B),
    queue(Q0),
    send_to_all(broadcaster, low, B, Q0, Q).

handle_pulses(Q, L, H) :-
    handle_pulses(Q, 0, L, 0, H).

handle_pulses(Q, L, L, H, H) :- queue(Q).
handle_pulses(Q, AccL, Lows, AccH, Highs) :-
    queue_head(Pulse, Q0, Q),
    handle_pulse(Pulse, Q0, Q1),
    Pulse = _-_-State,
    ( State = low, NAccL #= AccL+1, NAccH #= AccH ;
      State = high, NAccL #= AccL, NAccH #= AccH+1 ),
    handle_pulses(Q1, NAccL, Lows, NAccH, Highs).

handle_pulse(_-Name-_, Q, Q) :-
    \+flipflop(Name, _, _), \+conjunct(Name, _, _).
handle_pulse(_-Name-high, Q, Q) :-
    flipflop(Name, _, _).
handle_pulse(_-Name-low, Q, Q0) :-
    flipflop(Name, Outputs, State),
    invert(State, Inv),
    send_to_all(Name, Inv, Outputs, Q, Q0),
    retract(flipflop(Name, _, _)),
    assertz(flipflop(Name, Outputs, Inv)).

handle_pulse(From-To-State, Q, Q0) :-
    conjunct(To, Outputs, States),
    select(From-_, States, S),
    select(From-State, NewStates, S),
    ( all_high(NewStates) -> Pulse = low ; Pulse = high ),
    send_to_all(To, Pulse, Outputs, Q, Q0),
    retract(conjunct(To, _, _)),
    assertz(conjunct(To, Outputs, NewStates)).

invert(low, high).
invert(high, low).

send_to_all(From, State, Modules, Q, Q0) :-
    foldl(send(From, State), Modules, Q, Q0).

send(From, State, To, Q, Q0) :-
    %write([From,State,To]), nl,
    queue_last(From-To-State, Q, Q0).

all_high(States) :- maplist(\X^(X=_-high), States).

parse --> parse_from(From), " -> ", parse_to(To), {assert_module(From, To)}, parse.
parse --> [].

parse_from(broadcaster) --> "broadcaster".
parse_from(flipflop(L)) --> ['%', A, B], {atom_chars(L, [A,B])}.
parse_from(conjunct(L)) --> ['&', A, B], {atom_chars(L, [A,B])}.

parse_to([L|T]) --> [A, B], ", ", {atom_chars(L, [A,B])}, parse_to(T).
parse_to([L]) --> [A, B], "\n", {atom_chars(L, [A,B])}.

assert_module(broadcaster, To) :-
    assertz(broadcaster(To)).
assert_module(flipflop(L), To) :-
    assertz(flipflop(L, To, low)).
assert_module(conjunct(L), To) :-
    assertz(conjunct(L, To)).

prep_conjuncts :-
    findall(C-To, conjunct(C, To), Cs),
    maplist(prep_conjunct, Cs),
    retractall(conjunct/2).

% assumption: broadcaster doesnt send to conjunction directly
prep_conjunct(C-To) :-
    findall(L-low, ((flipflop(L, X, _) ; conjunct(L, X)), member(C, X)), Inputs),
    assertz(conjunct(C, To, Inputs)).
