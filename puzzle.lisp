;FUNCTION TO CONVERT FROM LIST OF NUMBER TO STRING
; '(1 2 3 4) => 1234
(defun numlist-to-string (lst)
  (when lst
    (concatenate 'string
                 (write-to-string (car lst)) (numlist-to-string (cdr lst)))))
 

; FUNCTION CREATE NODE
; input: (2 1 3 4 5 6 7 8 9) 3 )
; output: (213456789 (((1 1) 2) ((1 2) 1) ((1 3) 3) ((2 1) 4) ((2 2) 5) ((2 3) 6) ((3 1) 7) ((3 2) 8) ((3 3) 9)))
(defun create_node (l num)

(setq laux '()) ; auxiliar list
(setq node "") ; nuevo nodo
(setq posx 1) ; position X
(setq posy 1); position Y

(dolist (n l) ; loop new list

 (setq laux (cons (list (list posx posy ) n) laux))  
 (setq posy (+ posy 1))
 (setq node (concatenate 'string node (write-to-string n)))
 
 (if (> posy num)
   (setq posx (+ posx 1)))
 
 (if (> posy num)  
   (setq posy 1))
 
)

(setq node (parse-integer node)) ; convert a list of numbers into a number

(setq laux (reverse laux))

(setq laux (list node laux))

(return-from create_node laux)

)

; CLASS NODE: I use this class to save all the information related to a node
(defclass node()
  ((heuristic :accessor node-heuristic) ;h(node)
   (cost      :accessor node-cost) ; g(node)
   (parent    :accessor node-parent) ; pointer to its father node
   (num_node  :accessor node-num_node) ; 123498576
   (tiles     :accessor node-tiles) ; ((1 1) 2) ((1 2) 1) ((1 3) 3) ((2 1) 4) ((2 2) 5) ((2 3) 6) ((3 1) 7) ((3 2) 8) ((3 3) 9)
   (tiles-thash :accessor node-tiles-thash)) ; ;For example: 12 => 1 11 => 2 13 => 3 21 => 4 22 => 5 23 => 6 31 => 7 32 => 8 33 => 9
)

; CLASS GRAPH: I use this class to save start and goal node, dimesion of the puzzle (3 x 3)  and a table hash with the goal state  
(defclass graph()
 
  ((node-start :accessor graph-node-start) ; node_start
   (node-goal  :accessor graph-node-goal) ; node_goal
   (node_heu   :accessor graph-node-heu) ; table hash of tiles with value (x,y) of the last state or result
   (dimension  :accessor graph-dimension)) ; dimension of the puzzle
)

; HEURISTICA MANHATTAN
(defmethod heur_manhattan (graph node-act)

; Manhattan distance

(setq l1 (node-tiles node-act)) ; get tiles = ((1 1) 2) ((1 2) 1) ((1 3) 3) ((2 1) 4) ((2 2) 5) ((2 3) 6) ((3 1) 7) ((3 2) 8) ((3 3) 9)

(setf node-heu (graph-node-heu graph)) ; get goal-node table hash  with values x y

(setq sumr 0) ; initialize

(setq b-tile (graph-dimension graph)) ; number for black tile

(setq b-tile (* b-tile b-tile)) ; for example: dimesion is 3, result is 9

(dolist (n l1)
   
  (setq index (car (cdr n))) ; get index for enter hash table, for example: ((1 1) 2), get number 2

  (setq p ( gethash index node-heu )) ; get point of hash table, index = 2 => result = (1 2)

  (setq valx (car p)) ; get value X goal = 1
   
  (setq valy (car (cdr p)) ) ; get value Y goal = 2

  (setq valx-act (car (car n)) ) ; get value X  start = 1
       
  (setq valy-act (car (cdr (car n)) ) ) ; get value Y start = 1
     
  (setq difx (- valx valx-act) ) ; get value X node actual = 0
   
  (setq dify (- valy valy-act) ) ; get value Y node actual = 1
   
  (setq sum (+ (abs difx) (abs dify))) ; sum absolute values = 1
 
  (if (< index b-tile) ; don't calculate the heuristic for black tiles
 
   (setq sumr (+ sumr sum)) ; sum

  )
)

(return-from heur_manhattan sumr)

)

; FUNCTION TO GET GOAL PAIR 
; input: 9
; output: (3 3)
; Return position where this tile should be when the puzzle is finished
(defmethod get-goal-pair (graph index)

  (setq pair (gethash index (graph-node-heu graph)))

  (return-from get-goal-pair pair)

)

; LINEAR HORIZONTAL CONFLICT
(defmethod linear-hori-conflict (graph node)

(setq sumr 0) ; initialize

; Tiles
 (setq tiles (node-tiles node)) ; el estado del puzzle, el nodo con todos los valores
                                ; get tiles = ((1 1) 2) ((1 2) 1) ((1 3) 3) ((2 1) 4) ((2 2) 5) ((2 3) 6) ((3 1) 7) ((3 2) 8) ((3 3) 9)

(setq b-tile (graph-dimension graph)) ; number for black tile

(setq b-tile (* b-tile b-tile)) ; for example: dimesion is 3, result is 9

; First Loop over rows
(loop for i from 1 to (graph-dimension graph) do

  ; Second Loop over columns to check if there is a liner conflict
  (loop for j from 1 to (- (graph-dimension graph) 1)  do
       
       (setf pair1 (list i j)) ; pair with current position
        ; parse-integer => Convert a list of numbers into a number
       (setf pair2 (get-goal-pair graph (gethash (parse-integer (numlist-to-string pair1)) (node-tiles-thash node) ))) ; pair with goal position

       (loop for p from (+ j 1) to (graph-dimension graph) do

             (setf next-pair1 (list i p)) ; pair next pair current position
              ; parse-integer => Convert a list of numbers into a number
             (setf next-pair2 (get-goal-pair graph (gethash (parse-integer (numlist-to-string next-pair1))  (node-tiles-thash node) ))) ; next-pair1 with goal position
             ; Two tiles a and b are in a linear conflict if they are in the same row or column ,also their goal positions are in the same row or column and the goal
             ; position of one of the tiles is blocked by the other tile in that row.
             ;2 tiles tm and tn are said to be in linear conflict if tm and tn stand in the same line
             ;(row or column) as the goal positions of tm and tn ,tm is to the left/ right of tn, and
             ;the goal position of tm is to the right/ left of that of tn.
             (if (and (= (car pair2) i) (= (car next-pair2) i)
                 (or (<= (car (cdr next-pair2)) (car (cdr pair1))) (>= (car (cdr pair2)) (car (cdr next-pair1)))) 
                 (/= (gethash (parse-integer (numlist-to-string pair1)) (node-tiles-thash node) ) b-tile ) 
                 (/= (gethash (parse-integer (numlist-to-string pair2)) (node-tiles-thash node) ) b-tile )                  
                 )
                 (setq sumr (+ sumr 1)))
       )
   )
)
(return-from linear-hori-conflict sumr)
)

; LINEAR VERTICAL CONFLICT
(defmethod linear-vert-conflict (graph node)

(setq sumr 0) ; initialize

; Tiles
(setq tiles (node-tiles node)) ; el estado del puzzle, el nodo con todos los valores
                                ; get tiles = ((1 1) 2) ((1 2) 1) ((1 3) 3) ((2 1) 4) ((2 2) 5) ((2 3) 6) ((3 1) 7) ((3 2) 8) ((3 3) 9)

(setq b-tile (graph-dimension graph)) ; number for black tile

(setq b-tile (* b-tile b-tile)) ; for example: dimesion is 3, result is 9

; First Loop over columns
(loop for j from 1 to (graph-dimension graph) do

  ; Second Loop over rows to check if there is a liner conflict
  (loop for i from 1 to (- (graph-dimension graph) 1)  do
       
       (setf pair1 (list i j)) ; pair with current position
        ; parse-integer => Convert a list of numbers into a number
       (setf pair2 (get-goal-pair graph (gethash (parse-integer (numlist-to-string pair1)) (node-tiles-thash node) ))) ; pair with goal position    

       (loop for p from (+ i 1) to (graph-dimension graph) do

             (setf next-pair1 (list p j)) ; pair next pair current position
             (setf next-pair2 (get-goal-pair graph (gethash (parse-integer (numlist-to-string next-pair1))  (node-tiles-thash node) ))) ; next-pair1 with goal position
             ; Two tiles a and b are in a linear conflict if they are in the same row or column ,also their goal positions are in the same row or column and the goal
             ; position of one of the tiles is blocked by the other tile in that row.
             ;2 tiles tm and tn are said to be in linear conflict if tm and tn stand in the same line
             ;(row or column) as the goal positions of tm and tn ,tm is to the left/ right of tn, and
             ;the goal position of tm is to the right/ left of that of tn.
             (if (and (= (car (cdr pair2)) j) (= (car (cdr next-pair2)) j) 

                 (or (<= (car next-pair2) (car pair1)) (>= (car pair2) (car next-pair1)) )

                 (/= (gethash (parse-integer (numlist-to-string pair1)) (node-tiles-thash node) ) b-tile ) 
                 (/= (gethash (parse-integer (numlist-to-string pair2)) (node-tiles-thash node) ) b-tile ) 
                 )                 
                   (setq sumr (+ sumr 1)))
       )
   )
)
(return-from linear-vert-conflict sumr)
)


; HEURISTICA LINEAR CONFLICT
; calcular vertical and horizontal linear conflict
(defmethod heur_conflict_linear (graph node)

 (setq sum 0)

 (setq sum (+ sum (linear-vert-conflict graph node)))
 
 (setq sum (+ sum (linear-hori-conflict graph node)))

 (return-from heur_conflict_linear (* 2 sum))
 
)

; CALCULATE HEURISTIC
; Add manhattan and linear conflict heuristic
(defmethod calculate_heuristic (graph node)

(setf heu_man 0)

(setf heu_man (heur_manhattan graph node)) ; calculate heuristic manhattan for node-start

(setf heu_conflict 0)

(setf heu_conflict (heur_conflict_linear  graph node))

(return-from calculate_heuristic (+ heu_conflict heu_man))

)

; BUILD NODE
; build successor nodes
(defun  build-node (graph nodes_ht act-point move-point dim node openl-ht closel-ht)

; heuristic :accessor node-heuristic = h(node)
; cost      :accessor node-cost = g(node)
; parent    :accessor node-parent =  pointer to its father node
; num_node  :accessor node-num_node = 123498576
; tiles     :accessor node-tiles = ((1 1) 2) ((1 2) 1) ((1 3) 3) ((2 1) 4) ((2 2) 5) ((2 3) 6) ((3 1) 7) ((3 2) 8) ((3 3) 9)
; tiles-thash :accessor node-tiles-thash = For example: 12 => 1 11 => 2 13 => 3 21 => 4 22 => 5 23 => 6 31 => 7 32 => 8 33 => 9

(setf nodes_ht_aux (make-hash-table)) ; hashtable auxiliar

(maphash (lambda (key value)

(setf (gethash key nodes_ht_aux) value)
) nodes_ht) ; fill new hash table with the current state

; interchange of values between current tile and new tile
; e.g. 9 by 2. it always change the black tile for another one
(setq num1 (gethash move-point nodes_ht_aux))
(setq num2 (gethash act-point nodes_ht_aux))
(setf (gethash act-point nodes_ht_aux) num1)
(setf (gethash move-point nodes_ht_aux) num2)

; create a new node or new status
(setf new-node (make-instance 'node))
(setq num-node "")
 
(setq tiles '())
; loop through hash table nodes_ht_aux to create list with tiles
(loop for i from 1 to dim do

  (loop for j from 1 to dim do

(setq pxpy (concatenate 'string (write-to-string i) (write-to-string j))) ; concatenate x with y
 ; parse-integer => Convert a list of numbers into a number
(setq pxpy (parse-integer pxpy)) ; get key (x,y) like xy
(setf value (gethash pxpy nodes_ht_aux)) ; get value from tile from new state/configuration of the puzzle
(setq num-node (concatenate 'string num-node (write-to-string value))) ; create num_node e.g. 123456789
(setq tile (list (list i j) value)); create list of tiles (1 2) 3
(setq tiles (cons tile tiles)) ; concatena previous tile with the new element
  )
)

(setq tiles (reverse tiles))
(setq num-node (parse-integer num-node))

; If this node is in close list, not create this node
(if (not (eq (gethash num-node closel-ht) nil))
    (return-from build-node nil)
)


(setf (node-num_node new-node) num-node)  ; num-node e.g. 123456
(setf (node-tiles new-node) tiles) ; tiles e.g. ((1 1) 2) ((1 2) 1) ((1 3) 3) ((2 1) 4) ((2 2) 5) ((2 3) 6) ((3 1) 7) ((3 2) 8) ((3 3) 9)
(setf (node-tiles-thash new-node) nodes_ht_aux) ;hash table e.g. 12 => 1 11 => 2 13 => 3 21 => 4 22 => 5 23 => 6 31 => 7 32 => 8 33 => 9
(setf (node-parent new-node) node) ; pointer to its father node
(setf (node-cost new-node) (+ (node-cost node) 1)) ; g(x) = g(y) + 1, where x is the node and y is its father

; If this node is in open list, reuse the heuristic, which was already calculated and save in open list
(setf node-in-openl-ht (gethash (node-num_node new-node) openl-ht))
(if (eq node-in-openl-ht nil)
    (setf (node-heuristic new-node)(calculate_heuristic graph new-node)) ; heuristic else
    (setf (node-heuristic new-node)(node-heuristic node-in-openl-ht)) ; heuristic else
)

(return-from build-node new-node)

)
; GET NEXT SUCCESSORS NODES
(defun get_next_nodes(graph node dim open-list-hash-table close-list-hash-table)

(setf nodes-ht (make-hash-table))
(setq b-tiles (* dim dim))

; create hash table with all points and its values
(dolist (n (node-tiles node))
   
   (setq value (car (cdr n))) ; get value, number of the tile
   (setq x (car (car n))) ; get x
   (setq y (car (cdr (car n)))) ; get y
   (setq xy (concatenate 'string (write-to-string x) (write-to-string y))) ; concatenate x with y
    ; parse-integer => Convert a list of numbers into a number
   (setq xy (parse-integer xy)) ; get key (x,y) like xy
   (setf (gethash xy nodes-ht) value) ;insert key and value in hash table
   (if (= value (* dim dim)) ; get value of the black tile
      (setq point (car n)))
   (if (= value (* dim dim)) ; get position of the black tile
      (setq pointxy xy))

)

(setq pxpy "") ; initialize new point pxpy
(setq list_node '()) ; initialize list of nodes

; move left
(setq posx (car point)) ; get x
(setq posy (- (car (cdr point)) 1)) ; get y
(setq pxpy (concatenate 'string (write-to-string posx) (write-to-string posy))) ; concatenate x with y
 ; parse-integer => Convert a list of numbers into a number
(setq pxpy (parse-integer pxpy)) ; get key (x,y) like xy
(setq num (gethash pxpy nodes-ht)) ; get value in xy

(if (not (eql num NIL)) ; if the point is inside of the puzzle
    (progn
      (setf node-created (build-node graph nodes-ht pointxy pxpy dim node open-list-hash-table close-list-hash-table))
      (if (not(eq node-created nil ))
          (setq list_node (cons node-created list_node  ))
      )
    ) ; create node and put it inside of the list
)
; move right

(setq posx (car point)) ; get x
(setq posy (+ (car (cdr point)) 1)) ; get y
(setq pxpy (concatenate 'string (write-to-string posx) (write-to-string posy))) ; concatenate x with y
(setq pxpy (parse-integer pxpy)) ; get key (x,y) like xy
(setq num (gethash pxpy nodes-ht))  ; get value in xy
(if (not (eql num NIL)) ; if the point is inside of the puzzle
    (progn
      (setf node-created (build-node graph nodes-ht pointxy pxpy dim node open-list-hash-table close-list-hash-table))
      (if (not(eq node-created nil ))
          (setq list_node (cons node-created list_node  ))
      )
    ) ; create node and put it inside of the list
)

; move up

(setq posx (- (car point) 1)) ; get x
(setq posy (car (cdr point))) ; get y
(setq pxpy (concatenate 'string (write-to-string posx) (write-to-string posy))) ; concatenate x with y
(setq pxpy (parse-integer pxpy)) ; get key (x,y) like xy
(setq num (gethash pxpy nodes-ht))
(if (not (eql num NIL)) ; if the point is inside of the puzzle
    (progn
      (setf node-created (build-node graph nodes-ht pointxy pxpy dim node open-list-hash-table close-list-hash-table))
      (if (not(eq node-created nil ))
          (setq list_node (cons node-created list_node  ))
      )
    ) ; create node and put it inside of the list
)

; move down

(setq posx (+ (car point) 1)) ; get x
(setq posy (car (cdr point))) ; get y
(setq pxpy (concatenate 'string (write-to-string posx) (write-to-string posy))) ; concatenate x with y
(setq pxpy (parse-integer pxpy)) ; get key (x,y) like xy  
(setq num (gethash pxpy nodes-ht))  ; get value in xy
(if (not (eql num NIL)) ; if the point is inside of the puzzle
    (progn
      (setf node-created (build-node graph nodes-ht pointxy pxpy dim node open-list-hash-table close-list-hash-table))
      (if (not(eq node-created nil ))
          (setq list_node (cons node-created list_node  ))
      )
    ) ; create node and put it inside of the list
)

(return-from get_next_nodes list_node)

)

; BUILD SOLUTION (PATH)
(defmethod path(node-result)

  (setq node-aux node-result) ; goal node

  (setq list-result '()) ; list with the results
 
  (loop

   (setq list-result (cons (node-num_node node-aux) list-result))

   (when (eq (node-parent node-aux) nil) (return-from path list-result))

   (setq node-aux (node-parent node-aux)) ; set parent and  going back

  )

)

; FUNCTION A*
(defmethod astar(graph)

(setq open-list (make-instance 'cl-heap:priority-queue)) ; create open list like queue
                                                                    ; for get the best f(x)(make-instance 'cl-heap:priority-queue)

(setq open-list-ht (make-hash-table)) ; open-list: nodes that has not visited

(setq close-list-ht (make-hash-table)) ; close-nodes: nodes that already has been visited

; START NODE

(setf node-start (graph-node-start graph)) ; get node-start of the graph

(setq num-node-start (node-num_node node-start)) ; set node number of node-start,
    ; e.g. 134628759

(setq num-node-goal (node-num_node (graph-node-goal graph))) ; set node nnumber of node-goal e.g. 123456789

(setf heur-value (calculate_heuristic graph node-start)) ; calculate heuristic

(setf (node-heuristic node-start) heur-value) ; assign heuristic to start nodede

(setf fx (+ (node-heuristic node-start) (node-cost node-start))) ; f(x) = node-start.cost + node-start.heuristic

(cl-heap:enqueue open-list num-node-start fx) ; Enqueue the item num-node-start with the priority of fx.

(setf (gethash num-node-start open-list-ht) node-start) ; add to hash table the start node

(loop

   (when (= 0 (cl-heap:queue-size open-list)) (return-from astar '())) ;  run until open list is empty
   
   (setf num-current-node (cl-heap:dequeue open-list)) ; get node with min fx
   
   (if (eq (gethash num-current-node close-list-ht) nil)

        (progn    

            (setq current-node (gethash num-current-node open-list-ht)) ; get node from open list

            (if (= num-node-goal num-current-node) ; path to goal found, we found goal node
                (return-from astar (path current-node)) )  
            
            (remhash num-current-node open-list-ht) ; delete current node from open-list

            (setf (gethash num-current-node close-list-ht) T) ; add current-node to close-list

            (setq list-nodes (get_next_nodes graph current-node (graph-dimension graph) open-list-ht close-list-ht  )) ; get next nodes from current node

            (loop for n in list-nodes do

                  (if (eq (gethash (node-num_node n) open-list-ht) nil) ; in open
                      (progn
                        (setf (gethash (node-num_node n) open-list-ht) n) ; add a new node to open list
                        (cl-heap:enqueue open-list (node-num_node n) (+ (node-cost n) (node-heuristic n) )) ; at to the queue                         
                      )   
                      (progn

                        (setq node-in-open-list (gethash (node-num_node n) open-list-ht )) ; get node to compare with new node
                                               
                        (if  (< (node-cost n) (node-cost node-in-open-list)) ; if heuristic of the node n is less than node in close-list                                                                                                                               
                            (progn
                              (setf (node-parent node-in-open-list) (node-parent n)) ; set node-parent from new one to node in open list
                              (setf (node-cost node-in-open-list) (node-cost n)) ; i also set cost, heuristic is the same                             
                              (cl-heap:enqueue open-list (node-num_node n) (+ (node-cost n) (node-heuristic n) )) ; set new value to the queue

                            )
                        )
                     )                                           
                  )  
            )
        )

    )
)

)


; MAIN PROGRAM

; Create list of nodes create_node function
; this state 213456789 becomes in (((1 1) 2) ((1 2) 1) ((1 3) 3) ((2 1) 4) ((2 2) 5) ((2 3) 6) ((3 1) 7) ((3 2) 8) ((3 3) 9)))
; and this 123456789 becomes in (((1 1) 1) ((1 2) 2) ((1 3) 3) ((2 1) 4) ((2 2) 5) ((2 3) 6) ((3 1) 7) ((3 2) 8) ((3 3) 9)))
(setq l3 '())
(setq l4 '())


(print (setq l3 (create_node '(1 2 9 4 5 3 6 7 8) 3 ))) ; initial node FIND SOLUTION
;(print (setq l3 (create_node '(9 1 2 4 5 8 6 7 3) 3 ))) ; initial node COULD BE A CASE OF INFEASIBLE CASE
;(print (setq l3 (create_node '(9 1 3 4 2 6 7 5 8) 3 ))) ; initial node FIND SOLUTION
;(print (setq l3 (create_node '(1 8 2 9 4 3 7 6 5) 3 ))) ; initial node FIND SOLUTION
;(print (setq l3 (create_node '(1 8 2 9 4 3 7 6 5) 3 ))) ; initial node FIND SOLUTION
;(print (setq l3 (create_node '(8 1 2 9 4 3 7 6 5) 3 ))) ; initial node NOT SOLVABLE INFEASIBLE CASE

(print (setq l4 (create_node '(1 2 3 4 5 6 7 8 9) 3 ))) ; goal node

; Create both instances of node
(setf node-start (make-instance 'node))
(setf node-goal (make-instance 'node))

; Node start
(setf (node-heuristic node-start) 0) ; heuristic = h(node)
(setf (node-cost node-start) 0) ; cost = g(node) and f(node) = h(node) + g(node)
(setf (node-parent node-start) nil) ; pointer to its father node
(setf (node-num_node node-start) (car l3)) ; 213456789
(setf (node-tiles node-start) (car (cdr l3))) ;((1 1) 2) ((1 2) 1) ((1 3) 3) ((2 1) 4) ((2 2) 5) ((2 3) 6) ((3 1) 7) ((3 2) 8) ((3 3) 9)

; create table hash for node start
(setq nodes (make-hash-table))
(dolist (n (car (cdr l3))) ; create table hash with key xy and value z, for example 11 => 2

(setq index (car (cdr n)))
(setf (gethash (parse-integer(numlist-to-string(car n))) nodes) index)
)
(setf (node-tiles-thash node-start) nodes)  

; Node goal
(setf (node-heuristic node-goal) 0) ; h(node)
(setf (node-cost node-goal) 0) ;  g(node)
(setf (node-parent node-goal) nil) ; pointer to its father node
(setf (node-num_node node-goal) (car l4)) ; 123456789
(setf (node-tiles node-goal) (car (cdr l4))) ; (((1 1) 1) ((1 2) 2) ((1 3) 3) ((2 1) 4) ((2 2) 5) ((2 3) 6) ((3 1) 7) ((3 2) 8) ((3 3) 9))

(setq node-heu (make-hash-table)) ; create hash table of nodes for calculate heuristic
;For example:
;1 => (1 1)
;2 => (1 2)
;3 => (1 3)
;4 => (2 1)
;5 => (2 2)
;6 => (2 3)
;7 => (3 1)
;8 => (3 2)
;9 => (3 3)

(dolist (n (car (cdr l4))) ; create table hash with value x and y of the goal
(setq index (car (cdr n)))
(setf (gethash index node-heu) (car n))
)

(setf graph (make-instance 'graph)) ; create an instance of graph class
(setf (graph-node-start graph) node-start) ; start node type of node class
(setf (graph-node-goal graph) node-goal) ; goal node type of node class
(setf (graph-node-heu graph) node-heu) ; assign the table hash node_heu calculated previously of node goal to calculate heuristics
                                       ; this table contains last state or result of the puzzle
(setf (graph-dimension graph) 3) ; dimesion is 3 or num

(print "START PROGRAM =========================================================================")
(setf list (astar graph))
(setf dim (graph-dimension graph))
(print "Start state")
(print (node-num_node node-start))
(print "Goal State")
(print (node-num_node node-goal))
; print path from start node to 

(if (eq list'())
    (print "No solution was found. It could be a case of infeasible puzzle. The algorithm to detect these cases was not implemented for this solution")
)

(loop for n in list do

   (terpri)

   (setf tile-list (map 'list #'digit-char-p (prin1-to-string n)))

   (setf pos 1)
 
   (loop for tile in tile-list do

         (write tile)

         (if (= (rem pos dim) 0)
             (terpri))
     
         (setf pos (+ pos 1))

   )

   (terpri)

)


; MAIN PROGRAM FINISH