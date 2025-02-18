%%%% <Pirovano> <Davide> <894632>
%%%% <Collaborazione: <Bonfanti> <Luca> <894394>>

*** Abbiamo assunto che il costo del percorso da un vertice
verso se stesso sia 0 ***

Gestione Errori:
Quando si verifica un errore il programma restituisce
l'errore e fallisce.

Errori Gestiti:
1. Creazione di un nuovo grafo con un nome di un grafo già
   presente nella base di dati.
	Errore: Un grafo con tale nome esiste già.

2. Creazione di un vertice con un nome di un vertice già
   presente nella base di dati.
	Errore: Il vertice %w è già presente nel grafo %w.

3. Creazione di un arco tra due vertici già presente nella
   base di dati.
	Errore: Tale arco è già presente nel grafo.

4. Creazione di un arco tra due vertici con peso negativo.
	Errore: Il peso dell'arco deve essere positivo.

5. Aggiornamento del peso di un arco con un nuovo peso negativo.
	Errore: Il peso deve essere positivo.

6. Aggiornamento del peso di un arco, ma tale arco non esiste
   nella base di dati.
	Errore: Tale arco non esiste.

7. Esecuzione dijkstra_sssp su un vertice che non ha vicini.
	Errore: Non è possibile raggiungere alcun nodo.

8. Esecuzione shortest_path senza aver eseguito dijkstra_sssp
   in precedenza.


%---------%---------%---------%---------%---------%---------%------%
% INTERFACCIA PROLOG PER LA MANIPOLAZIONE DEI GRAFI %

:- new_graph(G).
Questo predicato inserisce un nuovo grafo nella base-dati Prolog.

:- delete_graph(G).
Rimuove tutto il grafo (vertici e archi inclusi) dalla base-dati
Prolog.

:- new_vertex(G, V).
Aggiunge il vertice V nella base-dati Prolog. Nel caso in cui un
vertice sia già registrato per quel grafo segnala l'errore:
Il vertice V è già presente nel grafo G.

:- vertices (G, Vs).
Questo predicato è vero quando Vs è una lista contenente tutti
i vertici di G, formato-lista: [vertex(g, a), vertex(g, b), ...].

:- list_vertices(G).
Questo predicato stampa alla console dell’interprete Prolog la lista
dei vertici del grafo G.

:- new_edge(G, U, V, Weight).
Aggiunge un arco del grafo G alla base dati Prolog.

:- edges(G, Es).
Questo predicato è vero quando Es è una lista di tutti gli
archi presenti in G.

:- neighbors(G, V, Ns).
Questo predicato è vero quando V è un vertice di G e Ns è una
lista contenente gli archi che portano ai vertici N immediatamente
raggiungibili da V.

:- list_edges(G).
Questo predicato stampa alla console dell’interprete Prolog la
lista degli archi del grafo G.

:- list_graph(G).
Questo predicato stampa alla console dell’interprete Prolog la lista
dei vertici e la lista degli archi del grafo G.


---- NON RICHIESTI DALLA CONSEGNA MA AGGIUNTI DA NOI ----

:- vertices_list (G, Vs).
Questo predicato è vero quando Vs è una lista contenente tutti
i vertici di G, formato-lista: [a, b, c, ...].

:- vertex_neighbors(G, V, VNs).
Questo predicato controlla se G è un grafo e se V è un vertice di G,
inserisce in un una lista tutti i vertici "vicini" a V, e poi da
questa lista rimuove i vertici già visitati
(quindi con distanza già calcolata e minima dalla sorgente).

:- remove_from_list(Vs1, Vs2, Vs).
Questo predicato rimuove gli elementi della lista Vs2 dalla lista
Vs1, e restituisce il tutto in Vs, viene richiamato in
vertex_neighbors.



%---------%---------%---------%---------%---------%---------%------%
% SSSP IN PROLOG %

:- distance(G, V, D).
Questo predicato è vero quando V è un vertice di G e, durante e dopo
l’esecuzione dell’algoritmo di Dijkstra, la distanza minima del
vertice V dalla “sorgente” è D.

:- visited(G, V).
Questo predicato è vero quando V è un vertice di G e, durante e
dopo l’esecuzione dell’algoritmo di Dijkstra, V risulta “visitato”.

:- previous(G, V, U).
Questo predicato è vero quando V ed U sono vertici di G e, durante
e dopo l’esecuzione dell’algoritmo di Dijkstra, il vertice U è il
vertice “precedente” a V nel cammino minimo dalla “sorgente” a V.

:- change_distance(G, V, NewDist).
Questo predicato ha sempre successo con due effetti collaterali:
prima tutte le istanze di distance(G, V, _) sono ritirate dalla
base-dati Prolog, e quindi distance(G, V, NewDist) è asserita.

:- change_previous(G, V, U).
Questo predicato ha successo con due effetti collaterali: prima
tutte le istanze di previous(G, V, _) sono ritirate dalla base-dati
Prolog, e quindi previous(G, V, U) è asserita.

:- dijkstra_sssp((G, Source).
Questo predicato ha successo con un effetto collaterale.
Dopo la sua prova, la base-dati Prolog ha al suo interno i predicati
distance(G, V, D) per ogni V appartenente a G; la base-dati Prolog
contiene anche i predicati previous(G, V, U) e visited(V)
per ogni V, ottenuti durante le iterazioni dell’algoritmo
di Dijkstra.

:- shortest_path(G, Source, V, Path).
Questo predicato è vero quando Path è una lista di archi
 [edge(G, Source, N1, W1),
  edge(G, N1, N2, W2),
  …,
  edge(G, NK, V, WK)]
che rappresenta il “cammino minimo” da Source a V.


---- NON RICHIESTI DALLA CONSEGNA MA AGGIUNTI DA NOI ----

:- dijkstra(G, Source).
Questo predicato esegue le prime operazioni, necessarie per il
funzionamento dell'algoritmo di dijkstra tra le quali la creazione
di liste e assegnamento di valori alle entry.

:- dijkstra_algorithm(G, Vs)
Questo predicato esegue effettivamente la parte operativa
dell'algoritmo di dijkstra in maniera ricorsiva.
Calcolo della testa, cancellazione dallo heap, inserimento nella
base dati dei "visited".

:- weight_update_control(G, V, Ns).
Questo predicato controlla se la distanza provvisoria del nodo dalla
sorgente è minore rispetto alla somma tra vertice precedente e peso
dell'arco ed asserta nella base dati la minore.

:- delete_dist(G).
Questo predicato cancella tutte le precedenti asserzioni di
distance con grafo G presenti nella base dati, se presenti.

:- delete_visited(G).
Questo predicato cancella tutte le precedenti asserzioni di visited
con grafo G presenti nella base dati, se presenti.

:- delete_previous(G).
Questo predicato cancella tutte le precedenti asserzioni di
previous con grafo G presenti nella base dati, se presenti.

:- initialize_single_source(G, Source, Vs).
Questo predicato inserisce nella base dati i valori per il
funzionamento dell'algoritmo di dijkstra. Esplicita quale sia il
nodo sorgente e mette  distanza 0; per tutti gli altri nodi invece
asserta distanza infinita e il precedente ancora da definire.

:- path_list(G, Source, V, Ps).
Questo predicato va a inserire nella lista Ps la sequenza di archi
che specificano il percorso con peso minimo del grafo G, sorgente
Source e nodo finale V.



%---------%---------%---------%---------%---------%---------%------%
% MINHEAP IN PROLOG %

:- new_heap(H).
Questo predicato inserisce un nuovo heap nella base-dati Prolog.
Notate che il predicato heap(H, S) mantiene la dimensione corrente
dello heap nel secondo argomento.

:- list_heap(H).
Il predicato richiama listing/1 per stampare sulla console Prolog
lo stato interno dello heap.

:- delete_heap(H).
Rimuove tutto lo heap (incluse tutte le “entries”) dalla base-dati
Prolog.

:- heap_size(H, S).
Questo predicato è vero quanto S è la dimensione corrente dello
heap.

:- empty(H).
Questo predicato è vero quando lo heap H non contiene elementi.

:- not_empty(H).
Questo predicato è vero quando lo heap H contiene almeno un
elemento.

:- head(H, K, V).
Il predicato è vero quando l’elemento dello heap H con
chiave minima K è V.

:- insert(H, K, V).
Il predicato è vero quando l’elemento V è inserito nello heap H
con chiave K.
Naturalmente, lo heap H viene ristrutturato in modo da mantenere
la "heap property" per ogni nodo.

:- extract(H, K, V).
Il predicato è vero quando la coppia K, V con K minima, è rimossa
dallo heap H.
Naturalmente, lo heap H viene ristrutturato in modo da mantenere
la “heap property” per ogni nodo.

:- modify_key(H, NewKey, OldKey, V).
Il predicato è vero quando la chiave OldKey
(associata al valore V) è sostituita da NewKey.


---- NON RICHIESTI DALLA CONSEGNA MA AGGIUNTI DA NOI ----

:- father(F, C).
Il predicato controlla se F è genitore di C.

:- go_up(H, PC).
questo predicato viene utilizzato da insert e modify_key per
aggiornare lo stato dello heap in modo da rispettare la heap
property dopo che una entry è stata aggiunta o modificata.

:- ordering(H, Size).
Questo predicato serve al predicato list_heap/1 per stampare in
ordine
(dalla prima posizione all'ultima) le entry dello heap.

:- heapify(H, PF).
Questo predicato riordina le entry dello heap facendo in modo di
rispettare la "heap property".

:- swap(H, PF, PC).
Questo predicato scambia la posizione nello heap di due entry.
