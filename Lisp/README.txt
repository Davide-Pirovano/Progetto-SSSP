;; <Pirovano> <Davide> <894632>
;; <Collaborazione: <Bonfanti> <Luca> <894394>>

*** Abbiamo assunto che il costo del percorso da un vertice verso se stesso
sia 0 ***

Gestione Errori:
Quando si verifica un errore il programma restituisce l'errore e fallisce.

Errori Gestiti:
1. Creazione di un nuovo grafo con un nome di un grafo già presente nella 
	base di dati.
	Errore: Un grafo con tale nome esiste già.
2. Creazione di un vertice con un nome di un vertice già presente nella 
	base di dati.
	Errore: Il vertice è già presente nel grafo.
3. Creazione di un arco tra due vertici già presente nella base di dati.
	Errore: Tale arco è già presente nel grafo.
4. Creazione di un arco tra due vertici con peso negativo.
	Errore: Il peso dell'arco deve essere positivo.
5. Aggiornamento del peso di un arco con un nuovo peso negativo.
	Errore: Il peso deve essere positivo.
6. Aggiornamento del peso di un arco, ma tale arco non esiste nella 
	base di dati.
	Errore: Tale arco non esiste.
7. Esecuzione sssp-dijkstra su un vertice che non ha vicini.
	Errore: Non è possibile raggiungere alcun nodo.
8. Esecuzione sssp-shortest-path senza aver eseguito dijkstra_sssp 
	in precedenza.

;---------;---------;---------;---------;---------;---------;---------;
; INTERFACCIA COMMON LISP PER LA MANIPOLAZIONE DEI GRAFI ;

(is-graph graph-id) → graph-id or NIL 
Questa funzione ritorna il graph-id stesso se questo grafo è già stato creato,
oppure NIL se no. 

(new-graph graph-id) → graph-id
Questa funzione genera un nuovo grafo e lo
inserisce nel data base (ovvero nella hash-table) dei grafi. 

(delete-graph graph-id) → NIL 
Rimuove l’intero grafo dal sistema (vertici edges etc); ovvero rimuove tutte 
le istanze presenti nei data base (ovvero nelle hash-tables) del sistema.

(new-vertex graph-id vertex-id) → vertex-rep
Aggiunge un nuovo vertice vertex-id al grafo graph-id. 
Notate come la rappresentazione di un vertice associ un vertice ad un 
grafo (o più). 

(graph-vertices graph-id) → vertex-rep-list
Questa funzione torna una lista di vertici del grafo.

(new-edge graph-id vertex-id vertex-id &optional weight) → edge-rep 
Questa funzione aggiunge un edge del grafo graph-id nella hash-table *edges*.
La rappresentazione di un edge è
(edge graph-id u v weight)

(graph-edges graph-id) → edge-rep-list 
Questa funzione ritorna una lista una lista di tutti gli edges presenti 
in graph-id.

(graph-vertex-neighbors graph-id vertex-id) → vertex-rep-list 
Questa funzione ritorna una lista vertex-rep-list contenente gli edges,
(edge graph-id vertex-id N W), che portano ai vertici N immediatamente 
raggiungibili da vertex-id.

(graph-print graph-id) 
Questa funzione stampa alla console dell’interprete Common Lisp una lista
dei vertici e degli edges del grafo graph-id.

---- NON RICHIESTI DALLA CONSEGNA MA AGGIUNTI DA NOI ----

(is-vertex graph-id vertex-id) → vertex-rep or NIL
Questa funzione controlla se il vertex-id esiste ed appartiene al graph-id.

(is-edge graph-id vertex-1-id vertex-2-id) → edge-rep or NIL
Questa funzione controlla se il vertex-1-id esiste, se il vertex-2-id esiste
e se appartengono al graph-id; poi trova, se esiste, l'edge che ha come nodi
la coppia (vertex-1-id, vertex-2-id), oppure (vertex-2-id, vertex-1-id).

(not-visited-neighbors graph-id vertex-id) → vertex-rep-list 
Questa funzione ritorna una lista vertex-rep-list contenente gli edges non
visitati,(edge graph-id vertex-id N W), che portano ai vertici N
immediatamente raggiungibili da vertex-id.

(change-edge-weight graph-id vertex-id vertex-id weight) → edge-rep
Questa funzione permette di modificare il peso dell'arco tra i 2 vertici
specificati del grafo, aggiornando la hashtable *edges*.


;---------;---------;---------;---------;---------;---------;---------;
; SSSP IN COMMON LISP ;

(sssp-dist graph-id vertex-id) → d 
Questa funzione, dato un vertex-id di un grafo graph-id ritorna, durante e
dopo l’esecuzione dell’algoritmo di Dijkstra, la distanza minima d del vertice
vertex-id dalla “sorgente”.

(sssp-visited graph-id vertex-id) → boolean 
Questo predicato è vero quando vertex-id è un vertice di graph-id e, durante e
dopo l’esecuzione dell’algoritmo di Dijkstra, vertex-id risulta “visitato”.

(sssp-previous graph-id V) → U 
Questa funzione, durante e dopo l’esecuzione dell’algoritmo di Dijkstra, 
ritorna il vertice U che è il vertice “precedente” a V nel cammino minimo 
dalla “sorgente” a V.

(sssp-change-dist graph-id V new-dist) → NIL 
Questa funzione ha solo un effetto collaterale: alla chiave (graph-id V) 
nella hash-table *dist* viene associato il valore new-dist.

(sssp-change-previous graph-id V U) → NIL 
Questa funzione ha solo un effetto collaterale: alla chiave (graph-id V) 
nella hash-table *previous* viene associato il valore U.

(sssp-dijkstra graph-id source-id) → NIL 
Questa funzione termina con un effetto collaterale. Dopo la sua esecuzione,
la hash-table *dist* contiene al suo interno le associazioni (graph-id V) ⇒ d
per ogni V appartenente a graph-id; la hash-table *previous* contiene le
associazioni (graph-id V) ⇒ U; infine la hash-table *visited* contiene le 
associazioni (graph-id V) ⇒ {T, NIL}.

(sssp-shortest-path G Source V) → Path 
Questa funzione ritorna una lista di edge
((edge G Source N1 W1) 
(edge G N1 N2 W2)
 … 
(edge G NK V Wk)) 
che rappresenta il “cammino minimo” da Source a V.


---- NON RICHIESTI DALLA CONSEGNA MA AGGIUNTI DA NOI ----

(print-dist graph-id) → NIL
Stampa tutte le coppie valore contenute nella hash table *dist*.

(print-visited graph-id) → NIL
Stampa tutte le coppie valore contenute nella hash table *visited*.

(print-previous graph-id) → NIL
Stampa tutte le coppie valore contenute nella hash table *previous*.

(dijkstra graph-id source-id) → T
Questa funzione effettua le operazioni preliminari per l'esecuzione 
dell'algoritmo di Dijkstra come, svuotare le hash table collegate a 
graph-id (se necessario), poi lancia inizialize-single-source, 
e internal-dijkstra.

(initialize-single-source graph-id source-id vertices) → T
Questa funzione imposta la source-id come effettiva sorgente da cui 
calcolare le distanze, imposta la distanza della sorgente a 0. 
Per tutti gli altri vertici della lista vertices mette distanza 
MOST-POSITIVE-DOUBLE-FLOAT.
Setta per tutti i vertici visited a NIL e i previous a 'not-defined. 


(internal-dijkstra graph-id vertex-id vertices) → T
Questa funzione esegue effettivamente la parte operativa dell'algoritmo 
di dijkstra in maniera ricorsiva.
calcolo della testa, cancellazione dallo heap, inserimento nella 
hash table *visited* e richiama relax.

(weight_update_control graph-id vertex-id neighbors) → T
Questa funzione controlla se la distanza provvisoria del nodo dalla sorgente
è minore rispetto alla somma tra vertice precedente e peso dell'edge e
aggiunge la coppia chiave valore della minore nella hash table *dist*.

(prov-dist graph-id vertex-1-id vertex-2-id) → d
Questa funzione controlla se la distanza del vertex 2 dalla sorgente sommando 
distanza vertex-1 al peso dell'edge tra vertex1 e vertex-2.

(find-old-key heap-id i value) → key
Questa funzione, passata come parametro a modify-key all'interno di relax, 
restituisce la chiave associata al valore value.

(remove-from-list vertex-id lista-1) → lista-2
Questa funzione rimuove l'elemento vertex-id dalla lista1 la
prima volta che lo incontra.
È stato necessiario inserire questa funzione in quanto falliti molteplici 
volte i tentativi con le funzioni delete e remove fornite da Lisp.

(path-list graph-id source-id vertex-id) → lista
Questa funzione restituisce la lista contenente gli edges che ripercorrono
il cammino minimo da vertex-id a source-id.

;---------;---------;---------;---------;---------;---------;---------;
; MINHEAP IN COMMON LISP ;

(new-heap H &optional capacity) → heap-rep 
Questa funzione inserisce un nuovo heap nella hash-table *heaps*. 

(heap-delete heap-id) → T 
Rimuove tutto lo heap indicizzato da heap-id. 

(heap-empty heap-id) → boolean 
Questo predicato è vero quando lo heap heap-id non contiene elementi.

(heap-not-empty heap-id) → boolean 
Questo predicato è vero quando lo heap heap-id contiene almeno un elemento.

(heap-head heap-id) → (K V) 
La funzione heap-head ritorna una lista di due elementi dove K è la chiave 
minima e V il valore associato.

(heap-insert heap-id K V) → boolean 
La funzione heap-insert inserisce l’elemento V nello heap heap-id con chiave
K.

(heap-extract heap-id) → (K V) 
La funzione heap-extract ritorna la lista con K, V e con K minima; la coppia 
è rimossa dallo heap heap-id. 

(heap-modify-key heap-id new-key old-key V) → boolean 
La funzone heap-modify-key sostituisce la chiave OldKey 
(associata al valore V) con NewKey.

(heap-print heap-id) → boolean 
Questa funzione stampa sulla console lo stato interno dello heap heap-id.


---- NON RICHIESTI DALLA CONSEGNA MA AGGIUNTI DA NOI ----

(is-heap heap-id) → heap-id or NIL
Questa funzione controlla se esiste un heap di nome heap-id.

(go-up heap-id i key value) → T
questa funzione viene utilizzata da heap-insert e heap-modify-key per 
aggiornare lo stato dello heap in modo da rispettare la heap property dopo
che una entry è stata aggiunta o modificata.

(heapify heap-id i key value) → T
Questa funzione riordina le entry dello heap facendo in modo di rispettare
la "heap property".

(parent i) → d
Questa funzione restituisce la posizione del padre nell'array associato
allo heap.

(change-size heap-id amount) → T
Questa funzione modifica la dimensione dell'heap pari ad amount, amount 
può essere positivo o negativo.

(get-value heap-id i) → (k v)
Restituisce la coppia (k v) associato alla posizione i dello heap heap-id.

(set-value heap-id i value) → T
Assegna il valore value nella posizione i dello heap heap-id.

(get-index-key heap-id i key value) → i
Restituisce la posizione i in cui si trova la coppia (key value) all'interno
dello heap heap-id.

(array-print heap-id i) → T
Questa funzione stampa a schermo tutti gli elementi dell'array
dello heap heap-id.


