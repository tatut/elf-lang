:- module(elf_map, [map_new/1, map_put/4, map_get/3, map_pairs/2,
                    map_keys/2,
                    map_size/2]).
:- use_module(library(rbtrees)).

map_new(map(M)) :- rb_new(M).

map_put(map(M0), Key, Val, map(M1)) :- rb_insert(M0, Key, Val, M1), !.

map_get(map(M), Key, Val) :- rb_lookup(Key, Val, M) -> true; Val=nil.

map_pairs(map(M), Pairs) :-
    ground(Pairs),
    list_to_rbtree(Pairs, M), !.

map_pairs(map(M), Pairs) :-
    findall(K-V, rb_in(K,V,M), Pairs0),
    sort(Pairs0, Pairs).
map_keys(map(M), Keys) :-
    findall(K, rb_in(K,_V,M), Keys).

map_size(map(M), Size) :- rb_size(M, Size).
