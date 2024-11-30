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

methods_start(Out) :-
    write(Out, '<!DOCTYPE>\n<html><head><title>Elf method reference</title><style>table, th, td { border: solid 1px black; border-collapse: collapse; } thead { background-color: lightgray; } th, td { padding: 0.4rem; } td { white-space: pre-wrap; font-family: monospace; vertical-align: top; }</style></head><body><h3>Elf methods</h3><table><thead><tr><td>Name</td><td>Doc</td></tr></td></thead><tbody>').

methods_end(Out) :-
    write(Out, '</tbody></table></body></html>').

method_row(Out, Args) :-
    format(Out, '<tr><td>~s</td><td>~s</td></tr>', Args).

gen_methods :-
    setup_call_cleanup(
        open('methods.html', write, Out),
        (methods_start(Out),
         findall([Method, Doc], elf:method(Method/_, Doc), Methods),
         sort(Methods, MethodsSorted),
         maplist(method_row(Out), MethodsSorted),
         methods_end(Out)),
        close(Out)).
