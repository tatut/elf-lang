% Formatted output
:- module(fmt, [fmt/3]).

fmt(PatternCs, Args, Out) :-
    string_codes(Pattern, PatternCs),
    re_split('%(s|d|%)', Pattern, Splits),
    with_output_to_codes(fmt_(Splits,Args), Out).

spec("%s").
spec("%d").
spec("%%").

fmt_([],[]).
fmt_([],[_]) :- throw(extra_arguments_to_fmt).
fmt_(["%%"|Patterns], Args) :-
    write('%'),
    fmt_(Patterns, Args).
fmt_(["%s"|Patterns], [StrCs|Args]) :-
    string_codes(Str, StrCs),
    write(Str),
    fmt_(Patterns, Args).
fmt_(["%d"|Patterns], [Num|Args]) :-
    write(Num),
    fmt_(Patterns,Args).
fmt_([Str|Patterns], Args) :-
    \+ spec(Str),
    write(Str),
    fmt_(Patterns, Args).
fmt_([Spec|_], []) :-
    spec(Spec),
    throw(too_few_arguments_to_fmt).

printable(N) :- between(32, 126, N).

pretty(X) :- is_list(X), maplist(printable, X), string_codes(Str, X), write(Str), !.
pretty(X) :- write(X).
