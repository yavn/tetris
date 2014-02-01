;;;; Copyright 2014 Maciej Jesionowski
;;;; A tetris-like game in Clojure
;;;; This is purely for educational purposes. Comments are a bit excessive
;;;; because of that.
;;;;
;;;; Licensed under GNU GPL v3


(ns tetris.core
  (:gen-class)
  (:import [javax.swing JFrame JPanel Timer]
           [java.awt.event KeyEvent ActionListener KeyListener WindowListener]
           [java.awt Color]))

;; This program probably calls out for a few namespaces (files) to organize
;; the source. However, for the sake of simplicity I decided not to do that
;; and just dump everything in a single file...

(def game-grid-size { :rows 16 :cols 12 })
(def cell-size-in-pixels 32)
(def game-time-step-millis 1000)

;; Mutable game state. We choose refs because the grid and the falling
;; block must change in sync (e.g. block gets "flattened" onto the grid
;; and a new block is then created).
;; In practice syncing is not needed because everything will happen on
;; the same thread anyway (AWT-Event queue). So in pure Java we could've gotten
;; away with not using any synchronization. Clojure however forces us to
;; choose mutability semantics.

(def state-grid (ref nil))
(def state-block (ref nil))

;; Refs must be updated inside a transaction (the famous STM). This constitutes
;; a synchronous (blocks the calling thread) coordinated (all refs inside
;; the transaction represent the same point in time) update.

(def colors
  { :empty   Color/lightGray 
    :cyan    Color/cyan
    :blue    Color/blue
    :orange  Color/orange
    :yellow  Color/yellow
    :green   Color/green
    :magenta Color/magenta
    :red     Color/red })

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

(defn cells->pixels [n]
  (* n cell-size-in-pixels))

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

(defn grid-rotate
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

(defn make-random-block
  "Picks one of the shapes and creates a block out of it,
  placing it in the middle of zero row."
  []
  (let [random-shape (first (shuffle shapes))
        grid (make-grid-with-shape random-shape)
        block-width (grid-cols grid)
        ;; (long ...) is called a type coercion (in other words, a cast)
        ;; and is typically best avoided. Possible rare use cases are
        ;; performance improvements, or like in this case getting rid of
        ;; odd numbers.
        middle-col (long (/ (- (:cols game-grid-size) block-width) 2))]
    (make-block [0 middle-col] grid)))

(defn block-move
  "Moves a block to a new position by adding the displacement vector."
  [block displacement]
  (let [new-position (map + (:position block) displacement)]
    (assoc block :position new-position)))

(defn block-rotate
  "Rotates the block clockwise."
  [block]
  (assoc block :grid (grid-rotate (:grid block))))

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
  ;; Don't bother if block is outside the bounds. In this case when form will
  ;; evaluate to nil.
  (when (block-in-bounds? grid block)
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
                      ;; Now we only need to merge the cells. If neither is :empty
                      ;; a :collision will be returned.
                      (place-cell cell new-cell))))))]
      (let [result-grid (place-with-collisions grid block)]
        ;; A set #{} can act as a predicate. Check some's doc for more info.
        (if (some #{:collision} (flatten result-grid))
          nil
          result-grid)))))

(defn row-full?
  "Returns true if all cells in a row are non-empty."
  [row]
  ;; #() is a reader macro (syntax) to concisely introduce an unnamed function (lambda).
  ;; Predicate below is equivalent to (fn [x] (not= x :empty))
  ;; More than one argument is supported with %1, %2, etc. and variable arguments are
  ;; introduced with %&. Functions using this syntax are best kept short and simple.
  (every? #(not= % :empty) row))

(defn place-block-in-grid-with-row-removal
  "Same as place-block-in-grid but also removes full rows and collapses the
  grid as it happens. Will also return nil on invalid placements."
  [initial-grid block]
  (let [grid (place-block-in-grid initial-grid block)]
    (when grid
      ;; comp (compose functions) comes handy here. We turn row-full? predicate
      ;; into row-not-full?
      (let [filtered-grid (filter (comp not row-full?) grid)
            removed-rows-count (- (grid-rows grid) (grid-rows filtered-grid))
            missing-rows (make-grid removed-rows-count (grid-cols grid))]
        (into filtered-grid missing-rows)))))

(defn update-block
  ;; This is an update function for a ref. Check 'alter' doc for info.
  "Applies function f to the block and optional arguments with
    (apply f old-block args)
  If the updated block can be placed in the grid without collision then the
  new block is returned. Otherwise the old block is returned."
  [old-block f grid & args]
  ;; This is a nice example of a higher-order function (a function that takes
  ;; another function as an argument).
  ;; update-block encapsulates a common algorithm (updated block must fit without
  ;; collisions in the grid), but the specific update algorithm is defined by f.
  (let [new-block (apply f old-block args)]
    ;; If condition evaluates to nil (failed to place the block) keep
    ;; using the old (not modified) block.
    (if (place-block-in-grid grid new-block)
      new-block
      old-block)))

(defn can-block-fall-in-grid?
  "Moves the block one row lower. If it can't fall more, nil is returned and the
  original block should be placed at its current position."
  [grid block]
  (not= block (update-block block block-move grid [1 0])))

(defn update-game
  "Game logic update. Makes the block fall, etc."
  []
  (dosync
    (if (can-block-fall-in-grid? @state-grid @state-block)
      (alter state-block update-block block-move @state-grid [1 0])
      (do
        (ref-set state-grid (place-block-in-grid-with-row-removal @state-grid @state-block))
        (ref-set state-block (make-random-block))))))

;; A forward declaration. Timer is defined later, but we use it in handle-key.
;; This is a syntactic sugar for (def foo) i.e. without specifying foo's value.
(declare game-timer)

(defn handle-key [key-code]
  (cond
    (= key-code KeyEvent/VK_LEFT)
      (dosync (alter state-block update-block block-move @state-grid [0 -1]))
    (= key-code KeyEvent/VK_RIGHT)
      (dosync (alter state-block update-block block-move @state-grid [0 1]))
    (= key-code KeyEvent/VK_UP)
      (dosync (alter state-block update-block block-rotate @state-grid))
    (= key-code KeyEvent/VK_DOWN)
      (do
        ;; This is a bit ugly but we need to interrupt the timer when forcing the
        ;; block to fall. Otherwise timer tick my coincidence with a key press and we'd
        ;; get a block falling two rows at a time which feels inconsistent.
        (.restart game-timer)
        (update-game))))

(defn paint-grid
  ;; g is java.awt.Graphics2D object
  [g grid]
  (doseq [[idx-row grid-row] (indexed grid)]
    (doseq [[idx-col cell-color] (indexed grid-row)]
      (let [x (cells->pixels idx-col)
            y (cells->pixels idx-row)]
      (.setColor g (get colors cell-color))
      (.fillRect g x y cell-size-in-pixels cell-size-in-pixels)))))

(defn make-game-panel []
  (proxy [JPanel ActionListener KeyListener] []
    (paintComponent [g]
      ;; proxy-super is like Java's super call. Here it means we invoke
      ;; JPanel's implementation of paintComponent.
      (proxy-super paintComponent g)
      ;; Flatten the block onto the grid so it can be drawn together.
      ;; Just as in GIMP -- block is "a layer" :)
      (let [game-grid (place-block-in-grid @state-grid @state-block)]
        (paint-grid g game-grid)
        ;; If game-grid is nil then it means that block is in illegal position.
        ;; Usually this means that the grid has filled up and the game is over!
        (when-not game-grid
          (.setColor g Color/black)
          (.drawString g "Game over!" 16 24))))
    (actionPerformed [e] ; this method gets called each time the timer fires
      (update-game)
      ;; Proxy defines an implicit 'this' symbol.
      (.repaint this))
    (getPreferredSize []
      (java.awt.Dimension. (cells->pixels (:cols game-grid-size))
                           (cells->pixels (:rows game-grid-size))))
    (keyPressed [e]
      (handle-key (.getKeyCode e))
      (.repaint this))
    (keyReleased [e])
    (keyTyped [e])))

(defn reset-game []
  (dosync
    (ref-set state-grid (make-grid (:rows game-grid-size)
                                   (:cols game-grid-size)))
    (ref-set state-block (make-random-block))))

(defn -main
 [& args]
 ;; work around dangerous default behaviour in Clojure
 (alter-var-root #'*read-eval* (constantly false))
 (let [frame (JFrame. "Clojure Tetris")
       panel (make-game-panel)
       timer (Timer. game-time-step-millis panel)]
   ;; Even though def is inside -main and let it always changes the root binding of a symbol
   ;; and defines it at the global scope. Root binding is the value seen across all threads.
   (def game-timer timer)
   (doto panel
     (.setFocusable true)
     (.addKeyListener panel))
   (doto frame
     (.add panel)
     (.pack)
     (.addWindowListener
       ;; proxy is an amazing form that lets us succinctly extend a class
       ;; and implement interfaces. This makes writing Java in Clojure more
       ;; pleasant than writing Java in Java! Thank you, Clojure :)
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
   (reset-game)
   (.start timer)))
   