% Elf dev utilities, for Ace editor to get completions and such
:- module(elf_dev, []).

method_completion(PrefixStr, Completion, Doc) :-
    atom_string(Prefix, PrefixStr),
    method_completion_(Prefix, Completion, Doc).

method_completion_(Prefix, Completion, DocStr) :-
    elf:method(Completion/_, Doc),
    atom(Completion),
    atom_prefix(Completion, Prefix),
    string_codes(DocStr, Doc).

% A defined record field
% Code must be executed before this works and each execution cleans these up
method_completion_(Prefix, Completion, Doc) :-
    elf_record:record_field(_, _, Completion),
    atom_prefix(Completion, Prefix),
    format(string(Doc),
           '~w\nReturns value of field ~w\n\n~w(Val)\nReturns new instance of recipient with field ~w set to Val.',
           [Completion,Completion,Completion,Completion]).

% A defined method
method_completion_(Prefix, Completion, Doc) :-
    elf_record:record_method(_, Completion, _),
    atom_prefix(Completion, Prefix),
    format(string(Doc),
           '~w\n User defined method ¯\\_(ツ)_/¯', [Completion]).
