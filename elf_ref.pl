:- module(elf_ref, [ref_new/2, ref_set/2, ref_get/2]).
:- use_module(library(gensym)).
:- dynamic ref/2.

ref_new(ref(ID), Initial) :- gensym(ref, ID),
                             asserta(ref(ID, Initial)).

ref_get(ref(ID), Val) :- ref(ID, Val).
ref_set(ref(ID), NewVal) :-
    retractall(ref(ID, _)),
    asserta(ref(ID, NewVal)).
