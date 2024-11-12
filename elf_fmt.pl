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

pretty(_,X) :- is_list(X), maplist(printable, X), string_codes(Str, X),
               format('"~s"', [Str]), !.
pretty(_,[]) :- !, format('[]').
pretty(L,X) :- is_list(X), !, append(Items, [LastItem], X),
               succ(L, L1),
               format('['), maplist({L1}/[I]>>(pretty(L1,I), format(', ')), Items),
               pretty(L1,LastItem), format(']'), !.
pretty(0, nil) :- !. % output nothing on nil (toplevel)
pretty(_, nil) :- !, write('nil').
pretty(L, map(ID)) :- !, format('%{'),
                      succ(L, L1),
                      map_pairs(map(ID), AllPairs),
                      append(Pairs, [LastKey-LastVal], AllPairs),
                      maplist({L1}/[K-V]>>(pretty(L1,K), format(': '), pretty(L1,V), format(',\n  ')), Pairs),
                      pretty(L1,LastKey), format(': '), pretty(L1,LastVal), format('}').
pretty(L, rec(Record,D)) :-
    !, succ(L,L1),
    format('~w{', [Record]),
    pad(L, Record, Pad),
    findall(Field-Value, (record_field(Record, _, Field),
                          record_get(rec(Record,D), Field, Value)),
            FieldVals),
    once(append(FVs,[LastField-LastVal], FieldVals)),
    maplist({L1,Pad}/[F-V]>>(format('~w: ', [F]), pretty(L1,V), format('~s', [Pad])), FVs),
    format('~w: ',[LastField]), pretty(L1,LastVal), format('}').


pretty(_L, X) :- write(X).

pretty(X) :- pretty(0, X).

prettyln(X) :- pretty(X), nl.


pad(0, Atom, [44,10|Pad]) :-
    atom_length(Atom, L),
    L1 is L + 1,
    length(Pad, L1),
    maplist(=(32), Pad), !.
pad(_L, _, `, `) :- !.
