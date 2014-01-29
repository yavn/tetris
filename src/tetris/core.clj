;;;; Copyright 2014 Maciej Jesionowski
;;;; A tetris-like game in Clojure
;;;; This is purely for educational purposes. Comments are a bit excessive
;;;; because of that.
;;;;
;;;; Licensed under GNU GPL v3


(ns tetris.core
  (:gen-class)
  (:import [javax.swing JFrame JPanel]
           [java.awt.event KeyEvent]))

(def game-grid-size { :rows 14 :cols 10 })
(def cell-size-in-pixels 32)

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
;; I define a number of interface functions to manipulate these
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
  
(defn crop-grid
  "Return a cropped grid by removing a number of rows and columns from the
  top-left side of the grid."
  [grid rows cols]
  (for [r (drop rows grid)]  ; first we drop a number of rows
    (drop cols r)))          ; then drop a number of cols from each row 'r'

(defn place-block-in-grid
  "Create a new grid that has the block placed in a correct position.
  Returns nil if it's not possible to place the block."
  [grid block]
  (let [[row col] (:position block)
        cropped-grid (crop-grid grid row col)]
    ))

(defn -main
 [& args]
 ;; work around dangerous default behaviour in Clojure
 (alter-var-root #'*read-eval* (constantly false)))
