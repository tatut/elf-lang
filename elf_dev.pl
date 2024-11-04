% Elf dev utilities, for Ace editor to get completions and such
:- module(elf_dev, []).

method_completion(PrefixStr, Completion) :-
    atom_string(Prefix, PrefixStr),
    method_completion_(Prefix, Completion).

method_completion_(Prefix, Completion) :-
    elf:method(Completion/_),
    atom_prefix(Completion, Prefix).

% A defined record field
% Code must be executed before this works and each execution cleans these up
method_completion_(Prefix, Completion) :-
    elf_record:record_field(_, _, Completion),
    atom_prefix(Completion, Prefix).

% A defined method
method_completion_(Prefix, Completion) :-
    elf_record:record_method(_, Completion, _),
    atom_prefix(Completion, Prefix).
