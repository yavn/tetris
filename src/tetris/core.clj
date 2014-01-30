;;;; Copyright 2014 Maciej Jesionowski
;;;; A tetris-like game in Clojure
;;;; This is purely for educational purposes. Comments are a bit excessive
;;;; because of that.
;;;;
;;;; Licensed under GNU GPL v3


(ns tetris.core
  (:gen-class)
  (:import [javax.swing JFrame JPanel Timer]
           [java.awt.event KeyEvent ActionListener KeyListener WindowListener]))

(def game-grid-size { :rows 14 :cols 10 })
(def cell-size-in-pixels 32)
(def game-time-step-millis 1000)

(defn cells->pixels [n]
  (* n cell-size-in-pixels))

(def colors
  { :empty   java.awt.Color/lightGray 
    :cyan    java.awt.Color/cyan
    :blue    java.awt.Color/blue
    :orange  java.awt.Color/orange
    :yellow  java.awt.Color/yellow
    :green   java.awt.Color/green
    :magenta java.awt.Color/magenta
    :red     java.awt.Color/red })

;; These are the shapes of blocks used in the game.
;; Convenient string definitions will be later converted
;; to a grid representation.

(def shapes
  [{:color :cyan
    :grid ["XXXX"]}
   {:color :blue
    :grid ["X__"
           "XXX"]}
   {:color :orange
    :grid ["__X"
           "XXX"]}
   {:color :yellow
    :grid ["XX"
           "XX"]}
   {:color :green
    :grid ["_XX"
           "XX_"]}
   {:color :magenta
    :grid ["_X_"
           "XXX"]}
   {:color :red
    :grid ["XX_"
           "_XX"]}])

;; Game data structures are typical Clojure collections.
;; We define a number of interface functions to manipulate these
;; structures, e.g. create, query etc.
;; I'm not sure if using records or something else is preferable.

(defn make-grid
  "Creates an empty grid which is a collection of rows,
  each cell filled with an :empty keyword."
  [rows cols]
  (repeat rows (repeat cols :empty)))

(defn grid-rows [grid]
  (count grid))

(defn grid-cols [grid]
  (count (first grid)))

(defn make-grid-with-shape
  "Takes a shape definition and returns a grid representing that shape.
  Shape must have this structure (example):
    {:color :red
     :grid  [\"XXX\"
             \"_X_\"]}
  Cells marked X will become a color and underscore will become :empty area."
  [shape]
  (let [color (:color shape)
        grid (:grid shape)]
  (for [row grid] (replace {\X color, \_ :empty}
                           row))))

(defn rotate-grid
  "Perform a clockwise rotation of the grid. This is similar to matrix
  transposition but also reverses the order of the rows."
  [grid]
  (apply map vector (reverse grid)))
  ;;
  ;; This is so terse that we should break it apart and see what's going on.
  ;;
  ;; grid - is a vector of vectors, e.g. [[1 2 3][4 5 6]]
  ;; (reverse grid) - reverses elements of a collection, thus we get: [[4 5 6][1 2 3]]
  ;; apply - we use it to treat a collection as a list of arguments to a function.
  ;; (apply map vector [[4 5 6][1 2 3]]) - here map is the invoked function,
  ;;   vector is the first argument to map, and the vector [[4 5 6][1 2 3]] becomes
  ;;   the remaining argument list to map (always the last argument to apply).
  ;;   Apply breaks apart the outer vector as if we wrote this instead:
  ;;   (map vector [4 5 6] [1 2 3]).
  ;; map - takes a function (vector in this case) and applies it to first elements
  ;;   of each collection, then the second elements, etc. This gives us:
  ;;   ((vector 4 1)(vector 5 2)(vector 6 3))
  ;; vector - takes a number of arguments and turns them into a vector
  ;;
  ;; Wow. So little code to accomplish so much :)

(defn make-block
  "Create a tetris block."
  [position grid]
  ;; This is just a grid (which defines the shape) with a position (so we know
  ;; where to place it in the game area).
  {:position position :grid grid})
  
(defn indexed
  "Add an index to each element of the collection."
  [coll]
  (map vector (range (count coll)) coll))

(defn get-in-grid
  "Same as get-in but works for a grid. If grid is represented as
  a nested collection ordinary get-in wouldn't work. Returns :empty
  if indices are out of bounds."
  [grid [skip-rows skip-cols]]
  (if (or (< skip-rows 0) (< skip-cols 0))
    :empty
	  ;; ->> is a cool threading form which takes the first argument 'grid'
	  ;;   and runs it through consecutive functions. Really nice when
	  ;;   dealing with step-by-step transformations.
    (or (->> grid (drop skip-rows) first (drop skip-cols) first)
        :empty)))
  ;; Logical or in Clojure returns the first argument that evaluates to
  ;; true. We can use that as a neat coalesce function (return first
  ;; non-nil value) because nil is false and everything else is true.

(defn place-cell
  "Takes two grid cells and determines the result of merging them
  together. This is basically what happens when you place a block
  in the grid. If a cell is already occupied (i.e. not :empty)
  :collision will be returned."
  [c1 c2]
  (if (= c1 :empty)
    c2
    (if (= c2 :empty)
      c1
      :collision)))
  
(defn block-in-bounds?
  "Returns true if the block is inside the bounds of the grid."
  [grid block]
  (let [[row col] (:position block)]
    (if (or (< row 0) (< col 0))
      false
      (let [row-end (+ row (grid-rows (:grid block)))
            col-end (+ col (grid-cols (:grid block)))]
        (and (<= row-end (grid-rows grid))
             (<= col-end (grid-cols grid)))))))

(defn place-block-in-grid
  "Create a new grid that has the block placed in a correct position.
  Returns nil if it's not possible to place the block."
  [grid block]
  ;; Uhm, not sure if there's a nicer way to do this that is both
  ;; efficient and idiomatic.
  ;;
  ;; letfn lets us define local functions
  (letfn [(place-with-collisions
            [grid block]
            ;; These are the coordinates of the block in the grid.
            (let [[b-row b-col] (:position block)]
              ;; We use nested fors to add indices to each grid cell.
              (for [[row grid-row] (indexed grid)]
                (for [[col cell] (indexed grid-row)]
                  ;; At this point we have bound [row col cell] where 'row' and 'col' are
                  ;; indices into grid and 'cell' is the actual value in there.
                  (let [coords [(- row b-row) (- col b-col)]
                        new-cell (get-in-grid (:grid block) coords)]
                    ;; coords are row/col indices transformed into block's grid
                    ;; frame of reference.
                    ;; Now we only need to read block's cell and write it into the new
                    ;; grid if it's not nil nor :empty. Otherwise the old cell value
                    ;; is preserved.
                    (place-cell cell new-cell))))))]    
    (let [result-grid (place-with-collisions grid block)]
      ;; A set #{} can act as a predicate. Check some's doc for more info.
      (if (some #{:collision} (flatten result-grid))
        nil
        result-grid))))

(defn make-game-panel []
  (proxy [JPanel ActionListener KeyListener] []
    (paintComponent [g]
      (proxy-super paintComponent g))
    (actionPerformed [e]
      ;; frame update goes here
      (.repaint this))
    (getPreferredSize []
      (java.awt.Dimension. (cells->pixels (:cols game-grid-size))
                           (cells->pixels (:rows game-grid-size))))
    (keyPressed [e])
    (keyReleased [e])
    (keyTyped [e])))

(defn -main
 [& args]
 ;; work around dangerous default behaviour in Clojure
 (alter-var-root #'*read-eval* (constantly false))
 (let [frame (JFrame. "Clojure Tetris")
       panel (make-game-panel)
       timer (Timer. game-time-step-millis panel)]
   (doto panel
     (.addKeyListener panel))
   (doto frame
     (.add panel)
     (.pack)
     (.addWindowListener
       (proxy [WindowListener] []
         (windowActivated [e])
         (windowClosed [e]
           (.stop timer))
         (windowClosing [e])
         (windowDeactivated [e])
         (windowDeiconified [e])
         (windowIconified [e])
         (windowOpened [e])))
     (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
     (.setVisible true))
   (.start timer)))
   