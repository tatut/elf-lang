:- module(elf_file, [file_codes/2, file_lines/2]).

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

file_string(FileName, String) :-
    prolog_flag(elf_use_fetch, true), !,
    string_codes(FileStr, FileName),
    fetch(FileStr, text, String).

file_string(FileName, String) :-
    prolog_flag(elf_use_fetch, false), !,
    string_codes(FileStr, FileName),
    read_file_to_string(FileStr, String, []).

file_lines(FileName, LinesCs) :-
    file_string(FileName, String),
    split_string(String, "\n", "", LinesIn),
    ( last(LinesIn, "")
    -> append(Lines, [""], LinesIn)
    ; Lines = LinesIn),
    maplist(string_codes, Lines, LinesCs).
