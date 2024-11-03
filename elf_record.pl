:- module(elf_record, [record_field/3, record_method/3, record_data/3,
                       record_new/2, record_get/3, record_set/3,
                       get_record_method/5, record_cleanup/0,
                       record_dict/2]).
:- use_module(library(gensym)).

% Record definition: record_field(RecordName, Num, FieldName).
% Records are stored as compound terms and mutated with nb_arg.
% Fields also act as 0 arg methods that return the value
:- dynamic record_field/3.

% Record method definition: record_method(RecordName, MethodName, fun(...))
:- dynamic record_method/3.

% Record fields are stored in record_data(RecordInstanceId, Field, Val)
:- dynamic record_data/3.

record_new(Record, rec(Record,ID)) :- gensym(r, ID).

record_get(rec(_,ID), Field, Val) :-
    record_data(ID, Field, Val) -> true; Val=nil.
record_set(rec(_,ID), Field, Val) :-
    retractall(record_data(ID, Field, _)),
    asserta(record_data(ID, Field, Val)).

get_record_method(Record, Name, Fun, Args0, Args1) :-
    % Don't try methods that are field accessors
    \+ record_field(Record, _, Name),
    once(get_record_method_(Record, Name, Fun, Args0, Args1)).

get_record_method_(Record, Name, Fun, Args, Args) :-
    record_method(Record, Name, Fun).

get_record_method_(Record, Name, Fun, Args, [NameStr, Args]) :-
    record_method(Record, dynamic, Fun),
    atom_codes(Name, NameStr).

% Clear any stored record defs and data when program ends
record_cleanup :-
    retractall(record_field(_,_,_)),
    retractall(record_method(_,_,_)),
    retractall(record_data(_,_,_)).

record_dict(rec(Rec,ID), Dict) :-
    findall(Field-Val, record_data(ID, Field, Val), Pairs),
    dict_pairs(Dict, Rec, Pairs).
