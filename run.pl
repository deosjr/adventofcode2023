:- use_module(library(clpz)).
:- use_module(library(format)).
:- use_module(library(lists)).
:- use_module(library(pio)).

:- dynamic(use_test_input/0).

% the following command executed toplevel will run day 1
% scryer-prolog -g 'run(1),halt' run.pl
run(Day) :-
    prologfile(Day, PrologFile),
    consult(PrologFile),
    inputfile(Day, InputFile),
    run_day(Day, InputFile).

prologfile(Day, PrologFile) :-
    phrase(format_("~|~`0t~d~*+", [Day, 2]), Padded),
    append(Padded, Padded, FormatInput),
    phrase(format_("~w~w/day~w~w.pl", FormatInput), List),
    atom_chars(PrologFile, List).

inputfile(Day, InputFile) :-
    phrase(format_("~|~`0t~d~*+", [Day, 2]), Padded),
    ( use_test_input ->
        phrase(format_("~w~w/test", Padded), InputFile) ;
        append(Padded, Padded, FormatInput),
        phrase(format_("~w~w/day~w~w.input", FormatInput), InputFile)
    ).

write_part1(Answer) :-
    format("Part 1: ~w~n", [Answer]).

write_part2(Answer) :-
    format("Part 2: ~w~n", [Answer]).
