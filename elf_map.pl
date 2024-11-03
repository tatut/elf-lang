:- module(elf_map, [map_new/1, map_put/3, map_get/3, map_pairs/2,
                    map_size/2, map_cleanup/0]).
:- use_module(library(gensym)).

% Dynamic attribute to hold map data:
% map_data(ID, Key, Val)
:- dynamic map_data/3.

map_new(map(ID)) :- gensym(map, ID).

map_put(map(ID), Key, Val) :-
    retractall(map_data(ID, Key, _)),
    asserta(map_data(ID, Key, Val)).

map_get(map(ID), Key, Val) :-
    map_data(ID, Key, Val) -> true; Val=nil.

map_pairs(map(ID), Pairs) :-
    findall(K-V, map_data(ID, K,V), Pairs0),
    sort(Pairs0, Pairs).

map_size(map(ID), Size) :-
    aggregate_all(count, map_data(ID, _, _), Size).

map_cleanup :-
    retractall(map_data(_,_,_)).
