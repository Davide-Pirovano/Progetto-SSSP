; -*- Mode: Common Lisp -*-
; sssp.lisp


;---------;---------;---------;---------;---------;---------;---------;
; SINGLE SOURCE SHORTEST PATH ;


; progetto a cura di:

; Bonfanti Luca, Matricola: 894394
; Pirovano Davide, Matricola:  


;---------;---------;---------;---------;---------;---------;---------;
; HASH TABLE IN COMMON LISP ;


(defparameter *vertices* (make-hash-table :test #'equal))
(defparameter *edges* (make-hash-table :test #'equal))
(defparameter *graphs* (make-hash-table :test #'equal))
(defparameter *visited* (make-hash-table :test #'equal))
(defparameter *distances* (make-hash-table :test #'equal))
(defparameter *previous* (make-hash-table :test #'equal))

(defparameter *heaps* (make-hash-table :test #'equal))

; parametri ausiliari
(defparameter *lista* nil)
(defparameter *testa-heap* nil)
(defparameter *capacity-originale* nil)


;---------;---------;---------;---------;---------;---------;---------;
; INTERFACCIA COMMON LISP PER LA MANIPOLAZIONE DEI GRAFI ;

(defun is-graph (graph-id)
  (or 
   (gethash 
    graph-id
    *graphs*)
   nil))

(defun new-graph (graph-id)
  (or 
   (is-graph
    graph-id)
   (setf 
    (gethash 
     graph-id 
     *graphs*) 
    graph-id)))

; abbiamo utilizzato (declare (ignore v)) in quanto risultava unbound
(defun delete-graph (graph-id)
  (and 
   (remhash
    graph-id
    *graphs*)
   (maphash 
    #'
    (lambda (k v)
      (declare
       (ignore
        v))
      (if 
          (equal 
           (second
            k)
           graph-id)
          (remhash
           k
           *vertices*)
        T))
    *vertices*)
   (maphash 
    #' 
    (lambda (k v)
      (declare
       (ignore
        v))
      (if 
          (equal 
           (second
            k)
           graph-id)
          (remhash
           k
           *edges*)
        T))
    *edges*)))

(defun is-vertex (graph-id vertex-id)
  (or 
   (gethash 
    (list 
     'vertex
     graph-id
     vertex-id)
    *vertices*)
   nil))



(defun new-vertex (graph-id vertex-id)
  (or
   (is-vertex
    graph-id
    vertex-id)
   (setf
    (gethash
     (list
      'vertex
      graph-id
      vertex-id)
     *vertices*)
    (list
     'vertex
     graph-id
     vertex-id))))

(defun graph-vertices (graph-id)
  (or 
   (setq
    *lista*
    nil)
   (maphash 
    #' 
    (lambda (k v)
      (and
       (equal
        (second 
         k)
        graph-id)
       (setq
        *lista*
        (append
         (list
          v)
         *lista*))))
    *vertices*)
   *lista*))

(defun is-edge (graph-id vertex-1-id vertex-2-id)
  (or 
   (gethash
    (list
     'edge
     graph-id
     vertex-1-id
     vertex-2-id)
    *edges*)
   (gethash
    (list
     'edge
     graph-id
     vertex-2-id
     vertex-1-id)
    *edges*)
   nil))

(defun new-edge (graph-id vertex-1-id vertex-2-id &optional (weight 1))
  (or
   (is-edge
    graph-id
    vertex-1-id
    vertex-2-id)
   (setf
    (gethash
     (list
      'edge 
      graph-id
      vertex-1-id
      vertex-2-id)
     *edges*)
    (list
     'edge
     graph-id
     vertex-1-id
     vertex-2-id
     weight))))

(defun graph-edges (graph-id)
  (or
   (setq
    *lista*
    nil)
   (maphash
    #'
    (lambda (k v)
      (and 
       (equal 
        (second 
         k)
        graph-id)
       (setq
        *lista*
        (append
         (list
          v)
         *lista*))))
    *edges*)
   *lista*))

(defun graph-vertex-neighbors (graph-id vertex-id)
  (or 
   (setq 
    *lista*
    nil)
   (maphash 
    #' 
    (lambda (k v)
      (cond 
       ((and 
         (equal 
          (third 
           k) 
          vertex-id)
         (not
          (sssp-visited
           graph-id
           (fourth
            k))))
        (setq
         *lista*
         (append
          *lista*
          (list
           v))))
       ((and
         (equal
          (fourth
           k)
          vertex-id)
         (not
          (sssp-visited
           graph-id
           (third
            k))))
        (setq
         *lista*
         (append 
          *lista*
          (list 
           v))))
       (T
        T)))                         
    *edges*)
   *lista*))

(defun graph-print (graph-id)
  (or 
   (maphash 
    #' 
    (lambda (k v)
      (and
       (equal
        (second
         k) 
        graph-id)
       (print 
        v)))
    *vertices*)
   (maphash 
    #'
    (lambda (k v)
      (and
       (equal
        (second 
         k)
        graph-id)
       (print
        v)))
    *edges*)))

;---------;---------;---------;---------;---------;---------;---------;
; SSSP IN COMMON LISP ;

(defun sssp-dist (graph-id vertex-id)
  (or 
   (gethash
    (list
     'distance
     graph-id
     vertex-id)
    *distances*)
   nil))

(defun print-dist (graph-id)
  (maphash 
   #' 
   (lambda (k v)
     (and
      (equal
       (second
        k)
       graph-id)
      (print
       (list
        k
        v))))
   *distances*))

(defun sssp-visited (graph-id vertex-id)
  (or 
   (gethash
    (list
     'visited
     graph-id
     vertex-id)
    *visited*)
   nil))

(defun print-visited (graph-id)
  (maphash 
   #'
   (lambda (k v)
     (and
      (equal
       (second
        k)
       graph-id)
      (print
       (list
        k
        v))))
   *visited*))

(defun sssp-previous (graph-id vertex-id) 
  (or
   (gethash
    (list
     'previous 
     graph-id
     vertex-id)
    *previous*)
   nil))


(defun print-previous (graph-id)
  (maphash
   #'
   (lambda (k v)
     (and
      (equal
       (second
        k)
       graph-id)
      (print
       (list 
        k
        v)))) 
   *previous*))

(defun sssp-change-dist (graph-id vertex-id new-dist)
  (setf 
   (gethash
    (list
     'distance
     graph-id
     vertex-id)
    *distances*)
   new-dist))

(defun sssp-change-previous (graph-id vertex-V-id vertex-U-id)
  (setf
   (gethash
    (list 
     'previous
     graph-id
     vertex-V-id)
    *previous*)
   vertex-U-id))


(defun sssp-dijkstra (graph-id source-id)
  (and
   (dijkstra
    graph-id
    source-id)
   nil))

(defun dijkstra (graph-id source-id)
  (and 
   (clrhash
    *visited*)
   (clrhash
    *distances*)
   (clrhash
    *previous*)
   (new-heap
    graph-id)
   (initialize-single-source
    graph-id
    source-id
    (graph-vertices
     graph-id))
   (internal-dijkstra
    graph-id
    source-id
    (graph-vertices
     graph-id))
   (heap-delete
    graph-id)))

(defun internal-dijkstra (graph-id vertex-id vertices)
  (if
      (null
       vertices)
      T
    (and
     (setf 
      (gethash
       (list
        'visited
        graph-id
        vertex-id)
       *visited*)
      T)
     (relax
      graph-id 
      vertex-id
      (graph-vertex-neighbors
       graph-id
       (second
        (heap-extract
         graph-id))))
     (internal-dijkstra
      graph-id
      (second
       (heap-head
        graph-id))
      (remove-from-list
       vertex-id
       vertices)))))

(defun relax (graph-id vertex-id neighbors)
  (cond
   ((null
     neighbors)
    T)
   ;caso in cui arco ('edge G V U)
   ((equal
     (third
      (first
       neighbors))
     vertex-id)
    (if 
        (<=
         (gethash
          (list
           'distance
           graph-id
           (fourth
            (first
             neighbors)))
          *distances*)
         (prov-dist
          graph-id
          vertex-id
          (fourth
           (first
            neighbors))))
        (relax
         graph-id
         vertex-id
         (cdr 
          neighbors))
      (and 
       (sssp-change-dist
        graph-id
        (fourth
         (first
          neighbors))
        (prov-dist
         graph-id
         vertex-id
         (fourth
          (first
           neighbors))))
       (sssp-change-previous
        graph-id
        (fourth
         (first
          neighbors))
        vertex-id)
       (heap-modify-key
        graph-id
        (prov-dist
         graph-id
         vertex-id
         (fourth
          (first
           neighbors)))
        (find-old-key
         graph-id
         0
         (fourth
          (first
           neighbors)))
        (fourth
         (first
          neighbors)))
       (relax
        graph-id
        vertex-id
        (cdr
         neighbors)))))
   ;caso in cui arco ('arc G U V)
   (T 
    (if
        (<=
         (gethash
          (list
           'distance
           graph-id
           (third
            (first
             neighbors)))
          *distances*)
         (prov-dist
          graph-id
          vertex-id
          (third
           (first
            neighbors))))
        (relax
         graph-id
         vertex-id
         (cdr
          neighbors))
      (and
       (sssp-change-dist
        graph-id
        (third
         (first
          neighbors))
        (prov-dist
         graph-id
         vertex-id
         (third
          (first
           neighbors))))
       (sssp-change-previous
        graph-id
        (third
         (first
          neighbors))
        vertex-id)
       (heap-modify-key
        graph-id
        (prov-dist
         graph-id
         vertex-id
         (third
          (first
           neighbors)))
        (find-old-key
         graph-id
         0
         (third
          (first
           neighbors)))
        (third
         (first
          neighbors)))
       (relax
        graph-id
        vertex-id
        (cdr
         neighbors)))))))


(defun prov-dist (graph-id vertex-1-id vertex-2-id)
  (if 
      (gethash 
       (list
        'edge
        graph-id
        vertex-1-id
        vertex-2-id)
       *edges*)
      (+
       (gethash
        (list
         'distance
         graph-id
         vertex-1-id)
        *distances*)
       (fifth
        (gethash
         (list
          'edge 
          graph-id
          vertex-1-id
          vertex-2-id)
         *edges*)))
    (+ 
     (gethash
      (list
       'distance
       graph-id
       vertex-1-id)
      *distances*)
     (fifth
      (gethash
       (list
        'edge
        graph-id
        vertex-2-id
        vertex-1-id)
       *edges*)))))

(defun find-old-key (heap-id i value)
  (if 
      (=
       i
       (heap-size
        (gethash 
         heap-id
         *heaps*)))
      nil
    (if 
        (equal 
         (second
          (aref
           (heap-actual-heap
            (gethash
             heap-id
             *heaps*))
           i))
         value) 
        (first
         (aref
          (heap-actual-heap
           (gethash
            heap-id
            *heaps*))
          i))
      (find-old-key
       heap-id
       (+
        i
        1)
       value))))

;implementata perchè remove non dava i risultati aspettati
(defun remove-from-list (vertex-id list)
  (cond 
   ((null 
     list)
    nil)
   ((equal
     (cdr
      list)
     nil)
    (if
        (equal
         (third
          (first
           list))
         vertex-id) 
        nil 
      list))
   (T 
    (if
        (equal
         (third
          (first
           list))
         vertex-id)
        (rest
         list)
      (append
       (list 
        (first
         list))
       (remove-from-list
        vertex-id
        (rest
         list)))))))

(defun initialize-single-source (graph-id source-id vertices)
  (cond  
   ((null
     vertices) 
    T)
   ((equal
     source-id
     (third
      (first
       vertices)))
    (and
     (heap-insert
      graph-id
      0
      source-id)
     (sssp-change-dist
      graph-id
      source-id
      0)
     (not
      (setf
       (gethash
        (list
         'visited
         graph-id
         source-id)
        *visited*)
       'nil))
     (initialize-single-source 
      graph-id 
      source-id 
      (cdr
       vertices))))
   (T 
    (and
     (heap-insert
      graph-id
      9999
      (third
       (first
        vertices)))
     (sssp-change-dist
      graph-id
      (third
       (first
        vertices))
      9999)
     (not
      (setf
       (gethash 
        (list
         'visited
         graph-id
         (third
          (first
           vertices)))
        *visited*)
       'nil))
     (sssp-change-previous
      graph-id
      (third
       (first
        vertices))
      'not-defined)
     (initialize-single-source
      graph-id
      source-id
      (cdr
       vertices))))))


(defun sssp-shortest-path (graph-id source-id vertex-id) 
  (or 
   (sssp-dijkstra 
    graph-id
    source-id)
   (path-list
    graph-id
    source-id
    vertex-id)))


(defun path-list (graph-id source-id vertex-id)
  (or
   (setq
    *lista*
    nil)
   (cond
    ((equal
      (sssp-previous
       graph-id
       vertex-id)
      'not-defined)
     (and
      (print
       "Path does not exist") 
      nil))
    ((equal
      source-id
      (gethash
       (list
        'previous
        graph-id
        vertex-id)
       *previous*))
     (or
      (and
       (gethash
        (list
         'edge
         graph-id
         source-id
         vertex-id)
        *edges*)
       (setq
        *lista*
        (append
         (list
          (gethash
           (list
            'edge
            graph-id
            source-id
            vertex-id)
           *edges*)) 
         *lista*)))
      (and
       (gethash
        (list
         'edge
         graph-id
         vertex-id
         source-id)
        *edges*) 
       (setq
        *lista*
        (append
         (list
          (gethash
           (list
            'edge
            graph-id
            vertex-id
            source-id)
           *edges*))
         *lista*)))))
    (T 
     (or
      (and
       (gethash
        (list
         'edge
         graph-id
         (sssp-previous
          graph-id
          vertex-id)
         vertex-id)
        *edges*)
       (setq
        *lista*
        (append
         (path-list
          graph-id
          source-id
          (sssp-previous
           graph-id
           vertex-id)) 
         (list
          (gethash
           (list
            'edge
            graph-id
            (sssp-previous
             graph-id
             vertex-id)
            vertex-id)
           *edges*)))))
      (and 
       (gethash
        (list
         'edge
         graph-id
         vertex-id
         (sssp-previous
          graph-id
          vertex-id))
        *edges*)
       (setq
        *lista*
        (append
         (path-list
          graph-id
          source-id
          (sssp-previous
           graph-id
           vertex-id)) 
         (list
          (gethash
           (list
            'edge
            graph-id
            vertex-id
            (sssp-previous
             graph-id
             vertex-id))
           *edges*))))))))
   *lista*))


;---------;---------;---------;---------;---------;---------;---------;
; MINHEAP IN COMMON LISP ;


(defun is-heap (heap-id)
  (or 
   (gethash
    heap-id
    *heaps*)
   nil))

(defun new-heap (heap-id &optional (capacity 42))
  (or
   (is-heap
    heap-id)
   (and
    (setq
     *capacity-originale*
     capacity)
    (setf
     (gethash
      heap-id
      *heaps*)
     (list
      'heap
      heap-id
      0 
      (make-array
       capacity))))))

(defun heap-id (heap-rep)
  (second
   heap-rep))


(defun heap-size (heap-rep)
  (third
   heap-rep))


(defun heap-actual-heap (heap-rep)
  (fourth
   heap-rep))

(defun heap-delete (heap-id)
  (remhash
   heap-id 
   *heaps*))


(defun heap-empty (heap-id)
  (=
   0
   (heap-size
    (gethash
     heap-id
     *heaps*))))


(defun heap-not-empty (heap-id)
  (not
   (heap-empty
    heap-id)))

(defun heap-head (heap-id)
  (if
      (heap-not-empty
       heap-id)
      (get-value
       heap-id
       0)
    nil))

(defun heap-insert (heap-id key value)
  (and 
   (is-heap
    heap-id)
   (cond
    ((=
      (heap-size
       (gethash
        heap-id
        *heaps*))
      (array-total-size
       (heap-actual-heap
        (gethash
         heap-id
         *heaps*))))
     (print
      "Error: heap overflow"))
    (T 
     (and
      (go-up 
       heap-id
       (heap-size
        (gethash
         heap-id
         *heaps*))
       key
       value)
      (change-size
       heap-id
       1)
      (heap-adjust
       heap-id))))))

(defun go-up (heap-id i key value)
  (if 
      (and
       (<
        0
        i)
       (>
        (car
         (get-value
          heap-id
          (parent
           i)))
        key))
      (and
       (set-value
        heap-id
        i
        (aref
         (heap-actual-heap
          (gethash
           heap-id 
           *heaps*))
         (parent
          i)))
       (go-up
        heap-id
        (parent
         i)
        key
        value))
    (set-value
     heap-id
     i
     (list
      key
      value))))

(defun heap-extract (heap-id)
  (if 
      (heap-not-empty
       heap-id)
      ;then primo if
      (and 
       (setq
        *testa-heap*
        (heap-head
         heap-id))
       (if
           (=
            1
            (heap-size
             (gethash
              heap-id
              *heaps*)))
           ;then secondo if
           (and 
            (change-size
             heap-id
             (-
              1))
            (heap-adjust
             heap-id)
            (not 
             (set-value
              heap-id
              0
              nil))
            *testa-heap*)
         ;else secondo if
         (and 
          (change-size 
           heap-id 
           (-
            1))
          (heap-adjust
           heap-id)
          (set-value
           heap-id
           0
           (get-value
            heap-id
            (heap-size
             (gethash
              heap-id
              *heaps*))))
          (not
           (set-value
            heap-id
            (heap-size
             (gethash
              heap-id
              *heaps*))
            nil))
          (heapify
           heap-id
           0
           (first 
            (heap-head
             heap-id))
           (second
            (heap-head
             heap-id)))
          *testa-heap*)))
    ;else primo if
    (print 
     "Error: heap underflow")))

(defun heapify (heap-id i key value)
  (cond 
   ; caso nessun figlio
   ((<
     (heap-size
      (gethash
       heap-id
       *heaps*)) 
     (+
      1
      (+ 
       1
       (*
        2
        i))))
    T)
   ; caso figlio unico
   ((= 
     (heap-size
      (gethash
       heap-id 
       *heaps*)) 
     (+
      1
      (+
       1
       (*
        2
        i))))
    (if 
        (<
         (car
          (get-value 
           heap-id 
           (+
            1
            (*
             2
             i))))
           (car
            (get-value
             heap-id
             i)))
        (and 
         (set-value
          heap-id
          i
          (get-value
           heap-id
           (+
            1
            (*
             2
             i))))
         (set-value
          heap-id
          (+
           1
           (*
            2
            i))
          (list
           key
           value)))
      (set-value
       heap-id
       i
       (list
        key
        value))))
   ; caso due figli
   (T 
    (if
        (<
         (car
          (get-value
           heap-id 
           (+ 
            1
            (*
             2
             i))))
         (car
          (get-value
           heap-id 
           (+
            2 
            (*
             2
             i)))))
        (if (<
             (car
              (get-value
               heap-id
               (+
                1
                (*
                 2
                 i))))
             (car
              (get-value
               heap-id
               i)))
            (and
             (set-value
              heap-id
              i
              (get-value
               heap-id
               (+ 
                1 
                (*
                 2
                 i))))
             (set-value
              heap-id
              (+
               1
               (*
                2
                i))
              (list
               key
               value))
             (heapify
              heap-id
              (+
               1
               (*
                2
                i))
              (first
               (get-value
                heap-id
                (+
                 1
                 (*
                  2
                  i))))
              (second
               (get-value
                heap-id
                (+
                 1
                 (*
                  2
                  i))))))
          (set-value
           heap-id
           i
           (list
            key
            value)))
      (if 
          (<
           (car
            (get-value
             heap-id
             (+
              2
              (*
               2
               i))))
           (car
            (get-value
             heap-id
             i)))
          (and
           (set-value
            heap-id
            i
            (get-value
             heap-id
             (+
              2 
              (*
               2
               i))))
           (set-value
            heap-id
            (+
             2
             (*
              2
              i))
            (list
             key
             value))
           (heapify
            heap-id
            (+
             2
             (*
              2
              i))
            (first
             (get-value
              heap-id
              (+
               2
               (*
                2
                i))))
            (second
             (get-value
              heap-id
              (+
               2
               (*
                2
                i))))))
        (set-value
         heap-id
         i
         (list
          key
          value)))))))


(defun heap-modify-key (heap-id new-key old-key value)
  (if 
      (heap-not-empty
       heap-id)
      (and
       (set-value
        heap-id
        (get-index-key
         heap-id
         0
         old-key
         value)
        (list
         new-key
         value))
       (go-up
        heap-id
        (get-index-key
         heap-id
         0
         new-key
         value)
        new-key
        value)
       (heapify 
        heap-id
        (get-index-key
         heap-id
         0
         new-key
         value) 
        new-key
        value))
    (print 
     "Error: the heap is empty")))


(defun parent (i)
  (floor 
   (/
    (-
     i
     1)
    2)))


(defun change-size (heap-id amount)
  (setf 
   (gethash
    heap-id
    *heaps*)
   (list
    'heap
    heap-id
    (+
     amount
     (heap-size
      (gethash
       heap-id
       *heaps*)))
    (heap-actual-heap
     (gethash
      heap-id
      *heaps*)))))


(defun get-value (heap-id i)
  (aref
   (heap-actual-heap
    (gethash 
     heap-id
     *heaps*))
   i))


(defun set-value (heap-id i value)
  (setf 
   (aref 
    (heap-actual-heap 
     (gethash
      heap-id
      *heaps*))
    i)
   value))


(defun get-index-key (heap-id i key value)
  (if 
      (=
       i 
       (array-total-size
        (heap-actual-heap
         (gethash
          heap-id
          *heaps*))))
      nil
    (if
        (and 
         (=
          (first
           (get-value
            heap-id
            i))
          key)
         (equal
          (second
           (get-value
            heap-id
            i))
          value))
        i
      (get-index-key
       heap-id
       (+ 
        i
        1)
       key
       value))))


(defun heap-print (heap-id)
  (array-print
   heap-id
   0))


(defun array-print (heap-id i)
  (if
      (=
       i
       (heap-size
        (gethash
         heap-id
         *heaps*)))
      T
    (and
     (print
      (get-value
       heap-id
       i))
     (array-print
      heap-id
      (+
       i
       1)))))


(defun heap-adjust (heap-id)
  (cond
   ; caso heap 
   ((and
     (> 
      (array-total-size
       (heap-actual-heap
        (gethash
         heap-id
         *heaps*)))
      *capacity-originale*)
         (< 
          (heap-size
           (gethash
            heap-id
            *heaps*))
          (-
           *capacity-originale*
           1)))
    (and 
     (setf 
      (gethash
       heap-id
       *heaps*)
      (list 
       'heap 
       heap-id 
       (heap-size
        (gethash
         heap-id
         *heaps*))
       (adjust-array
        (heap-actual-heap
         (gethash
          heap-id
          *heaps*)) 
        *capacity-originale*)))
     T))
   ; caso heap quasi pieno
   ((= 
     (heap-size
      (gethash
       heap-id
       *heaps*))
     (-
      (array-total-size
       (heap-actual-heap
        (gethash
         heap-id
         *heaps*)))
      1))
    (and
     (setf
      (gethash
       heap-id
       *heaps*)
      (list
       'heap 
       heap-id 
       (heap-size
        (gethash
         heap-id
         *heaps*))
       (adjust-array
        (heap-actual-heap
         (gethash
          heap-id
          *heaps*)) 
        (+ 
         (array-total-size 
          (heap-actual-heap
           (gethash
            heap-id
            *heaps*)))
         5))))
     T))
   (T 
    T)))
