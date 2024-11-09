% Formatted output
:- module(elf_fmt, [fmt/3, pretty/1, prettyln/1, output/1, outputln/1]).
:- use_module(elf_map).
:- use_module(elf_record).
:- use_module(library(yall)).

fmt(PatternCs, Args, Out) :-
    string_codes(Pattern, PatternCs),
    re_split('%(s|d|w|%)', Pattern, Splits),
    with_output_to_codes(fmt_(Splits,Args), Out).

spec("%s").
spec("%d").
spec("%w").
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
fmt_(["%w"|Patterns], [Thing|Args]) :-
    write(Thing),
    fmt_(Patterns,Args).
fmt_([Str|Patterns], Args) :-
    \+ spec(Str),
    write(Str),
    fmt_(Patterns, Args).
fmt_([Spec|_], []) :-
    spec(Spec),
    throw(too_few_arguments_to_fmt).

printable(N) :- integer(N), between(32, 126, N).

output(X) :- is_list(X), maplist(printable, X), writef('%s',[X]), !.
output(nil) :- !. % nil prints nothing (unless pretty printing)
output(X) :- writef('%w', [X]).

outputln(X) :- output(X), nl.

pretty(X) :- is_list(X), maplist(printable, X), string_codes(Str, X),
             format('"~s"', [Str]), !.
pretty(X) :- is_list(X), append(Items, [LastItem], X),
             format('['), maplist([I]>>(pretty(I), format(', ')), Items),
             pretty(LastItem), format(']'), !.
pretty(nil) :- !. % output nothing on nil
pretty(map(ID)) :- format('%{'),
                   map_pairs(map(ID), AllPairs),
                   append(Pairs, [LastKey-LastVal], AllPairs),
                   maplist([K-V]>>(pretty(K), format(': '), pretty(V), format(',\n  ')), Pairs),
                   pretty(LastKey), format(': '), pretty(LastVal), format('}').
pretty(rec(Record,ID)) :-
    format('~w{', [Record]),
    pad(Record, Pad),
    findall(Field-Value, (record_field(Record, _, Field),
                          record_data(ID, Field, Value)),
            FieldVals),
    once(append(FVs,[LastField-LastVal], FieldVals)),
    maplist({Pad}/[F-V]>>(format('~w: ', [F]), pretty(V), format(',~n~s', [Pad])), FVs),
    format('~w: ',[LastField]), pretty(LastVal), format('}').


pretty(X) :- write(X).

prettyln(X) :- pretty(X), nl.

pad(Atom, Pad) :-
    atom_length(Atom, L),
    L1 is L + 1,
    length(Pad, L1),
    maplist(=(32), Pad).
