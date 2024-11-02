:- module(record, [record_field/3, record_method/3,
                  get_record_method/5]).

% Record definition: record_field(RecordName, Num, FieldName).
% Records are stored as compound terms and mutated with nb_arg.
% Fields also act as 0 arg methods that return the value
:- dynamic record_field/3.

% Record method definition: record_method(RecordName, MethodName, fun(...))
:- dynamic record_method/3.

get_record_method(Record, Name, Fun, Args0, Args1) :-
    % Don't try methods that are field accessors
    \+ record_field(Record, _, Name),
    once(get_record_method_(Record, Name, Fun, Args0, Args1)).

get_record_method_(Record, Name, Fun, Args, Args) :-
    record_method(Record, Name, Fun).

get_record_method_(Record, Name, Fun, Args, [NameStr, Args]) :-
    record_method(Record, dynamic, Fun),
    atom_codes(Name, NameStr).
