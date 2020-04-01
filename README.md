# puzzle-solution

<h3>Puzzle Solution<h3>

8-puzzle in Common Lisp using A* algorithm, Conflict linear and manhattan heuristic. The application was developed in LispWorks IDE. 

The programs save states in node objects and graph object (object oriented paradigm).

<h3> State representation <h3>
  
 We define two classes to save configurations of each state.
 
; CLASS NODE: I use this class to save all the information related to a node
(defclass node()
  ((heuristic :accessor node-heuristic) ;h(node)
   (cost      :accessor node-cost) ; g(node)
   (parent    :accessor node-parent) ; pointer to its father node
   (num_node  :accessor node-num_node) ; 123498576
   (tiles     :accessor node-tiles) ; ((1 1) 2) ((1 2) 1) ((1 3) 3) ((2 1) 4) ((2 2) 5) ((2 3) 6) ((3 1) 7) ((3 2) 8) ((3 3) 9)
   (tiles-thash :accessor node-tiles-thash)) ;For example: 12 => 1 11 => 2 13 => 3 21 => 4 22 => 5 23 => 6 31 => 7 32 => 8 33 => 9
)


; CLASS GRAPH: I use this class to save start and goal node, dimesion of the puzzle (3 x 3)  and a table hash with the goal state  
(defclass graph()
 
  ((node-start :accessor graph-node-start) ; node_start
   (node-goal  :accessor graph-node-goal) ; node_goal
   (node_heu   :accessor graph-node-heu) ; table hash of tiles with value (x,y) of the last state or result
   (dimension  :accessor graph-dimension)) ; dimension of the puzzle 1 => (1 1) 2 => (1 2) 3 => (1 3) 4 => (2 1) 5 => (2 2) 6 => (2 3)
                                           ; 7 => (3 1) 8 => (3 2) 9 => (3 3)
)
 
<h3> Algorithm A* <h3>
The algorithm used is detailed in the functional specification.
  
<h3> Infeasible Puzzle <h3>
8-puzzle problem has many configurations which can not be solved. The program doesn't identify this kind of states, but it informs to the users if the solution was not found (http://www.geeksforgeeks.org/check-instance-8-puzzle-solvable/)


  
