:- module(elf_file, [file_codes/2]).

% Set to true when using from browser
:- set_prolog_flag(elf_use_fetch, false).

file_codes(FileName, Codes) :-
    prolog_flag(elf_use_fetch, true), !,
    string_codes(FileStr, FileName),
    fetch(FileStr, text, Str),
    string_codes(Str, Codes),
    _ := console.log(Codes).

file_codes(FileName, Codes) :-
    prolog_flag(elf_use_fetch, false), !,
    atom_codes(F, FileName),
    read_file_to_codes(F, Codes, []).
