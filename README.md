# puzzle-solution

<h3>Puzzle Solution</h3>

8-puzzle solver in Common Lisp using A* algorithm with Conflict linear and manhattan heuristics. The application was developed in LispWorks IDE. I also used a priority queue to develop the program(https://common-lisp.net/project/cl-heap/). The program can be extended to run 16-puzzle. 

Tests performed shows a good performance of the solution, which uses manhattan and linear conflict heuristics together.

The program saves states in node objects and graph object (object oriented paradigm).

<h3> State representation </h3>
  
 I defined two classes to save configurations of each state.

```lisp
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
   ;1 => (1 1) 2 => (1 2) 3 => (1 3) 4 => (2 1) 5 => (2 2) 6 => (2 3) 7 => (3 1) 8 => (3 2) 9 => (3 3)
   (dimension  :accessor graph-dimension)) ; dimension of the puzzle 
)
```
<h3> Algorithm A* </h3>
The algorithm used is detailed in the functional specification.
  
<h3> Infeasible Puzzle </h3>
8-puzzle problem has many configurations which can not be solved. The program doesn't identify this kind of states, but it informs to the users if the solution was not found (http://www.geeksforgeeks.org/check-instance-8-puzzle-solvable/)

<h3> How to compile and run </h3>
1. Use quicklips to install the library cl-heap: <br>
   (quicklisp:quickload 'cl-heap) <br>
   (quicklisp:quickload 'cl-heap-tests) <br>
2. Load and Compile puzzle.lisp: <br>

Then the results are shown:
```lisp
"START PROGRAM =========================================================================" 
"Start state" 
129453678 
"Goal State" 
123456789 
129
453
678


192
453
678


912
453
678


412
953
678


412
653
978


412
653
798


412
693
758


412
963
758


912
463
758


192
463
758


129
463
758


123
469
758


123
496
758


123
456
798


123
456
789
```
Another option:
Run the file using the command "clisp puzzle.lisp" from the terminal.

In the main program, we can test other cases modifying the node start (number 9 is the blank space).

