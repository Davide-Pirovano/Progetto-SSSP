% Dichiarazioni

:- dynamic graph/1.
:- dynamic vertex/2.
:- dynamic edge/4.

:- dynamic distance/3.
:- dynamic visited/2.
:- dynamic previous/3.

:- dynamic heap/2.
:- dynamic heap_entry/4.


% Interfaccia Prolog per la manipolazione di grafi

new_graph(G) :-
    graph(G), !.

new_graph(G) :-
    assert(graph(G)), !.

delete_graph(G) :-
    graph(G),
    retractall(graph(G)),
    retractall(vertex(G, _)),
    retractall(edge(G, _, _, _)), !.

new_vertex(G, V) :-
    graph(G),
    vertex(G, V), !.

new_vertex(G, V) :-
    graph(G),
    assert(vertex(G, V)), !.

vertices(G, Vs) :-
    graph(G),
    findall(V, vertex(G, V), Vs), !.

list_vertices(G) :-
    graph(G),
    listing(vertex(G, _)), !.

new_edge(G, U, V) :-
    graph(G),
    vertex(G, U),
    vertex(V, U),
    new_edge(G, U, V, 1), !.

new_edge(G, U, V, Weight) :-
    graph(G),
    vertex(G, U),
    vertex(G, V),
    edge(G, U, V, Weight), !.

new_edge(G, U, V, Weight) :-
    graph(G),
    vertex(G, U),
    vertex(G, V),
    assert(edge(G, U, V, Weight)), !.

edges(G, Es) :-
    graph(G),
    findall(edge(G, U, V, W), edge(G, U, V, W), Es), !.

neighbors(G, V, Ns) :-
    graph(G),
    vertex(G, V),
    findall(edge(G, V, N, W), edge(G, V, N, W), Ns1),
    findall(edge(G, N, V, W), edge(G, N, V, W), Ns2),
    append(Ns1, Ns2, Ns), !.

list_edges(G) :-
    graph(G),
    listing(edge(G, _, _, _)), !.

list_graph(G) :-
    graph(G),
    list_vertices(G),
    list_edges(G), !.

% SSSP in Prolog DA-CONTROLLARE

distance(G, V, D) :-
    graph(G),
    vertex(G, V),
    number(D), !.

visited(G, V) :-
    graph(G),
    vertex(G, V), !.

previous(G, V, U) :-
    graph(G),
    vertex(G, V),
    vertex(G, U), !.

change_distance(G, V, NewDist) :-
    graph(G),
    vertex(G, V),
    retractall(distance(G, V, _)),
    assert(distance(G, V, NewDist)), !.

change_previous(G, V, U) :-
    graph(G),
    vertex(G, V),
    vertex(G, U),
    retractall(previous(G, V, _)),
    assert(previous(G, V, U)), !.

dijkstra_sssp(G, Source) :-
    graph(G),
    vertex(G, Source),
    dijkstra(G, Source), !. %qui si costruisce l'albero dei cammini minimi con l'algo TO-DO

shortest_path(G, Source, V, Path) :-
    graph(G),
    vertex(G, Source),
    vertex(G, V),
    dijkstra_sssp(G, Source),
    path_list(G, Source, V, Path), !. %stampa l'albero dei cammini minimi come lista TO-DO

% MINHEAP in Prolog

new_heap(H) :-
    heap(H, _), !.

new_heap(H) :-
    assert(heap(H, 0)), !.

delete_heap(H) :-
    retract(heap(H, _)),
    retractall(heap_entry(H, _, _, _)), !.

heap_size(H, S) :-
    heap(H, S), !.

empty(H) :-
    heap(H, 0), !.

not_empty(H) :-
    heap(H, _),
    heap_size(H, Size),
    Size > 0, !.

head(H, K, V) :-
    heap(H, _),
    not_empty(H),
    heap_entry(H, 1, K, V), !.

insert(H, K, V) :-
    heap(H, _),
    empty(H),
    change_size(H, 1),
    assert(heap_entry(H, 1, K, V)), !.

insert(H, K, V) :-
    heap(H, _),
    heap_size(H, Size),
    NewSize is Size + 1,
    father(PF, NewSize),
    heap_entry(H, PF, KF, _),
    KF =< K,
    change_size(H, 1),
    assert(heap_entry(H, NewSize, K, V)), !.

insert(H, K, V) :-
    heap(H, _),
    change_size(H, 1),
    heap_size(H, Size),
    assert(heap_entry(H, Size, K, V)),
    go_up(H, Size), !.


go_up(_, 1) :- !.

go_up(H, PC) :-
    heap_entry(H, PC, KC, _),
    father(PF, PC),
    heap_entry(H, PF, KF, _),
    KF =< KC, !.

go_up(H, PC) :-
    heap_entry(H, PC, _, _),
    father(PF, PC),
    swap(H, PF, PC),
    go_up(H, PF), !.

extract(H, _, _) :-
    heap(H, _),
    heap(H, Size),
    Size < 1, !.

% casi extract da aggiungere

modify_key(H, NewKey, OldKey, V) :-
    heap(H, _),
    heap_entry(H, _, OldKey, V),
    retract(heap_entry(H, P, OldKey, V)),
    assert(heap_entry(H, P, NewKey, V)),
    go_up(H, P),
    heapify(H, 1), !.

list_heap(H) :-
    heap(H, _),
    heap_size(H, Size),
    ordering(H, Size),
    listing(heap_entry(H, _, _, _)), !.

ordering(_, 0) :- !.

ordering(H, Size) :-
    retract(heap_entry(H, Size, K, V)),
    asserta(heap_entry(H, Size, K, V)),
    S is Size - 1,
    ordering(H, S), !.

swap(H, PF, PC) :-
    retract(heap_entry(H, PF, KF, VF)),
    retract(heap_entry(H, PC, KC, VC)),
    assert(heap_entry(H, PF, KC, VC)),
    assert(heap_entry(H, PC, KF, VF)).
