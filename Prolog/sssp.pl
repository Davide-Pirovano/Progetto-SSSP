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

vertex_neighbors(G, V, VNs) :-
    graph(G),
    vertex(G, V),
    findall(N, edge(G, V, N, W), Ns1),
    findall(N, edge(G, N, V, W), Ns2),
    append(Ns1, Ns2, Ns),
    findall(T, visited(G, T), Ts),
    remove_from_list(Ns, Ts, VNs).


remove_from_list(Vs1, [], Vs1) :- !.

remove_from_list(Vs1, Vs2, Vs) :-
    Vs2 = [T | Ts],
    member(T, Vs1),
    delete(Vs1, T, Us),
    remove_from_list(Us, Ts, Vs), !.

remove_from_list(Vs1, Vs2, Vs) :-
    Vs2 = [_ | Ts],
    remove_from_list(Vs1, Ts, Vs), !.


% SSSP in Prolog

distance(G, V, D) :-
    graph(G),
    vertex(G, V),
    number(D), !.

delete_distance(G) :-
    graph(G),
    retractall(distance(G, _, _)), !.

visited(G, V) :-
    graph(G),
    vertex(G, V), !.

delete_visited(G) :-
    retractall(visited(G, _)), !.

previous(G, V, U) :-
    graph(G),
    vertex(G, V),
    vertex(G, U), !.

delete_previous(G) :-
    retractall(previous(G, _, _)), !.

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
    delete_distance(G),
    delete_visited(G),
    delete_previous(G),
    dijkstra(G, Source), !. %qui si costruisce l'albero dei cammini minimi

dijkstra(G, Source) :-
    new_heap(G),
    % S <- vuoto
    delete_distance(G),
    delete_visited(G),
    delete_previous(G),
    % Q <- vertices(G)
    vertices(G, Vs),
    % initialize-single-source
    initialize_single_source(G, Source, Vs),
    % while Q non vuoto
    dijkstra_algorithm(G, Vs),
    delete_heap(G).

dijkstra_algorithm(_, []) :- !.

dijkstra_algorithm(G, Vs) :-
    % do u <- extract-min(Q)
    head(G, HK, HV),
    delete(Vs, HV, Ts),
    %Vs = [HV | Ts],
    extract(G, HK, HV),
    % S <- S U {u}
    assert(visited(G, HV)),
    %for each vertex v E Adj[v]
    vertex_neighbors(G, HV, Ns),
    %do weight update control(u, v, w)
    weight_update_control(G, HV, Ns),
    dijkstra_algorithm(G, Ts), !.

weight_update_control(_, _, []) :- !.

weight_update_control(G, V, Ns) :-
    Ns = [T | Ts],
    distance(G, V, DV),
    distance(G, T, DN),
    edge(G, V, T, W),
    DN =< DV + W,
    weight_update_control(G, V, Ts), !.

weight_update_control(G, V, Ns) :-
    Ns = [T | Ts],
    distance(G, V, DV),
    distance(G, T, DN),
    edge(G, T, V, W),
    DN =< DV + W,
    weight_update_control(G, V, Ts), !.

weight_update_control(G, V, Ns) :-
    Ns = [T | Ts],
    distance(G, V, DV),
    edge(G, V, T, W),
    NewDist is DV + W,
    change_distance(G, T, NewDist),
    change_previous(G, T, V),
    heap_entry(G, _, OldKey, T),
    modify_key(G, NewDist, OldKey, T),
    weight_update_control(G, V, Ts), !.

weight_update_control(G, V, Ns) :-
    Ns = [T | Ts],
    distance(G, V, DV),
    edge(G, T, V, W),
    NewDist is DV + W,
    change_distance(G, T, NewDist),
    change_previous(G, T, V),
    heap_entry(G, _, OldKey, T),
    modify_key(G, NewDist, OldKey, T),
    weight_update_control(G, V, Ts), !.


initialize_single_source(_, _, []) :- !.

initialize_single_source(G, Source, Vs) :-
    Vs = [Source | Ts],
    vertex(G, Source),
    assert(distance(G, Source, 0)),
    insert(G, 0, Source),
    initialize_single_source(G, Source, Ts), !.

initialize_single_source(G, Source, Vs) :-
    Vs = [T | Ts],
    assert(distance(G, T, inf)),
    assert(previous(G, T, not_defined)),
    insert(G, inf, T),
    initialize_single_source(G, Source, Ts), !.

shortest_path(G, Source, V, Path) :-
    graph(G),
    vertex(G, Source),
    vertex(G, V),
    dijkstra_sssp(G, Source),
    path_list(G, Source, V, Path), !. %stampa l'albero dei cammini minimi come lista

path_list(G, Source, V, Ps) :-
    previous(G, V, Source),
    edge(G, Source, V, W),
    Ps = [edge(G, Source, V, W)], !.

path_list(G, Source, V, Ps) :-
    previous(G, V, Source),
    edge(G, V, Source, W),
    Ps = [edge(G, Source, V, W)], !.

path_list(G, Source, V, Ps) :-
    previous(G, V, U),
    edge(G, U, V, W),
    path_list(G, Source, U, Ps1),
    append(Ps1, [edge(G, U, V, W)], Ps), !.

path_list(G, Source, V, Ps) :-
    previous(G, V, U),
    edge(G, V, U, W),
    path_list(G, Source, U, Ps1),
    append(Ps1, [edge(G, U, V, W)], Ps), !.

path_list(_, Source, V, _) :-
    writef("Path does not exist from %w to %w.", [Source, V]), !.

% MINHEAP in Prolog

new_heap(H) :-
    heap(H, _), !.

new_heap(H) :-
    assert(heap(H, 0)), !.


list_heap(H) :-
    heap(H, _),
    heap_size(H, Size),
    ordering(H, Size),
    listing(heap_entry(H, _, _, _)), !.


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

%min heapify
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

extract(H, K, V) :-
    heap(H, _),
    heap_entry(H, _, K, V),
    heap_size(H, 1),
    change_size(H, -1),
    retract(heap_entry(H, _, K, V)), !.

extract(H, K, V) :-
    heap(H, _),
    heap_entry(H, _, K, V),
    head(H, K, V),
    heap_size(H, Size),
    change_size(H, -1),
    retract(heap_entry(H, P, K, V)),
    retract(heap_entry(H, Size, K1, V1)),
    assert(heap_entry(H, P, K1, V1)),
    heapify(H, 1), !.


% nodo foglia
heapify(H, PF) :-
    heap_entry(H, PF, _, _),
    heap_size(H, Size),
    PC1 is PF * 2,
    PC1 > Size, !.

% figlio unico - figlio >= padre
heapify(H, PF) :-
    heap_entry(H, PF, KF, _),
    heap_size(H, Size),
    PC1 is PF * 2,
    PC1 = Size,
    heap_entry(H, PC1, KC1, _),
    KC1 >= KF, !.

% singolo figlio - figlio < padre
heapify(H, PF) :-
    heap_entry(H, PF, KF, _),
    heap_size(H, Size),
    PC1 is PF * 2,
    PC1 = Size,
    heap_entry(H, PC1, KC1, _),
    KC1 < KF,
    swap(H, PF, PC1),
    heapify(H, PC1), !.

% due figli - figli >= padre
heapify(H, PF) :-
    heap_entry(H, PF, KF, _),
    PC1 is PF * 2,
    heap_entry(H, PC1, KC1, _),
    KC1 >= KF,
    PC2 is PC1 + 1,
    heap_entry(H, PC2, KC2, _),
    KC2 >= KF, !.

% due figli - figlio 1 >= padre e figlio 2 < padre
heapify(H, PF) :-
    heap_entry(H, PF, KF, _),
    PC1 is PF * 2,
    heap_entry(H, PC1, KC1, _),
    KC1 >= KF,
    PC2 is PC1 + 1,
    heap_entry(H, PC2, KC2, _),
    KC2 < KF,
    swap(H, PF, PC2),
    heapify(H, PC2), !.

% due figli - figlio 1 < padre e figlio 2 >= padre
heapify(H, PF) :-
    heap_entry(H, PF, KF, _),
    PC1 is PF * 2,
    heap_entry(H, PC1, KC1, _),
    KC1 < KF,
    PC2 is PC1 + 1,
    heap_entry(H, PC2, KC2, _),
    KC2 >= KF,
    swap(H, PF, PC1),
    heapify(H, PC1), !.

% due figli - figlio 1 < padre e figlio 2 < padre (figlio 1 =< figlio 2)
heapify(H, PF) :-
    heap_entry(H, PF, KF, _),
    PC1 is PF * 2,
    heap_entry(H, PC1, KC1, _),
    KC1 < KF,
    PC2 is PC1 + 1,
    heap_entry(H, PC2, KC2, _),
    KC2 < KF,
    KC1 =< KC2,
    swap(H, PF, PC1),
    heapify(H, PC1), !.

 % due figli - figlio 1 < padre e figlio 2 < padre (figlio 2 < figlio 1)
heapify(H, PF) :-
    heap_entry(H, PF, KF, _),
    PC1 is PF * 2,
    heap_entry(H, PC1, KC1, _),
    KC1 < KF,
    PC2 is PC1 + 1,
    heap_entry(H, PC2, KC2, _),
    KC2 < KF,
    KC1 > KC2,
    swap(H, PF, PC2),
    heapify(H, PC2), !.


swap(H, PF, PC) :-
    retract(heap_entry(H, PF, KF, VF)),
    retract(heap_entry(H, PC, KC, VC)),
    assert(heap_entry(H, PF, KC, VC)),
    assert(heap_entry(H, PC, KF, VF)).


modify_key(H, NewKey, OldKey, V) :-
    heap(H, _),
    heap_entry(H, _, OldKey, V),
    retract(heap_entry(H, P, OldKey, V)),
    assert(heap_entry(H, P, NewKey, V)),
    go_up(H, P),
    heapify(H, 1), !.


father(F, C) :-
    F is floor(C / 2).


change_size(H, A) :-
    heap(H, _),
    heap(H, OldSize),
    NewSize is OldSize + A,
    retract(heap(H, OldSize)),
    assert(heap(H, NewSize)).


ordering(_, 0) :- !.

ordering(H, Size) :-
    retract(heap_entry(H, Size, K, V)),
    asserta(heap_entry(H, Size, K, V)),
    S is Size - 1,
    ordering(H, S), !.

% Test

test_1(G) :-
    new_graph(G),
    new_vertex(G, source),
    new_vertex(G, a),
    new_vertex(G, b),
    new_vertex(G, c),
    new_vertex(G, d),
    new_vertex(G, e),
    new_vertex(G, final),
    new_edge(G, a, b, 6),
    new_edge(G, source, a, 2),
    new_edge(G, source, d, 8),
    new_edge(G, a, c, 2),
    new_edge(G, d, c, 2),
    new_edge(G, d, e, 3),
    new_edge(G, c, e, 9),
    new_edge(G, e, final, 1),
    new_edge(G, b, final, 5),
    list_graph(G).
