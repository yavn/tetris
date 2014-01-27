(ns tetris.grid)

(def grid-size { :rows 14 :cols 10 })
(def cell-size-in-pixels 32)

(defn cells->pixels [n]
  (* n cell-size-in-pixels))

(def grid-dimension
  (java.awt.Dimension. (cells->pixels (:cols grid-size))
                       (cells->pixels (:rows grid-size))))

(def colors 
  { :empty   java.awt.Color/lightGray 
    :red     java.awt.Color/red
    :blue    java.awt.Color/blue
    :yellow  java.awt.Color/yellow
    :green   java.awt.Color/green
    :cyan    java.awt.Color/cyan
    :magenta java.awt.Color/magenta
    :orange  java.awt.Color/orange })

(def shapes
  [
   {:color :cyan
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
           "_XX"]}
   ])

(defprotocol GridProtocol
  (grid [this] "Cells in this grid, laid out in rows.")
  (rows [this])
  (cols [this]))

(defrecord Grid [data]
  GridProtocol
  (grid [_] data)
  (rows [_] (count data))
  (cols [_] (count (first data))))

(defrecord Shape [position grid]
  GridProtocol
  (grid [_] (grid grid))
  (rows [_] (rows grid))
  (cols [_] (cols grid)))

(defn make-grid
  ([] (make-grid (:rows grid-size) (:cols grid-size)))
  ([data]
    {:pre [(every? #(vector? %) data)
           (every? true? (for [row data] (every? keyword? row)))
           (let [counts (for [row data] (count row))
                 expected-count (/ (reduce + counts) (count counts))]
             (every? #(= expected-count %) counts))]}
    (->Grid data))
  ([rows cols]
    (->Grid
      (vec (repeat rows
                   (vec (repeat cols :empty)))))))

(defmacro iterate-over-grid 
  "Repeatedly executes body for each cell in the grid binding row and cell
  indices and cell value to the specified symbols. E.g.:

  (iterate-over-grid grid row-index col-index cell
    (println \"Cell at [\" row-index col-index \"] has value\" cell))"
  
  [grid row col cell & body]
  `(doseq [~row (range (rows ~grid)) ~col (range (cols ~grid))]
     (let [~cell (get-in (:data ~grid) [~row ~col])]
       ~@body)))

(defn paint [g grid]
  (iterate-over-grid grid row col color-name
                     (let [x (cells->pixels col)
                           y (cells->pixels row)]
                       (.setColor g (get colors color-name))
                       (.fillRect g x y cell-size-in-pixels cell-size-in-pixels))))

(defn place-shape [grid shape]
  (with-local-vars [grid-data (:data grid)]
    (iterate-over-grid (:grid shape) row col color-name
                       (let [[pos-row pos-col] (:position shape)
                             grid-pos [(+ row pos-row) (+ col pos-col)]]
                         (when (not= color-name :empty)
                           (var-set grid-data
                                 (assoc-in @grid-data grid-pos color-name)))))
    (make-grid @grid-data)))

(defn rotate-shape [shape]
  (let [shape-grid-data (:data (:grid shape))]
    (->Shape (:position shape)
             (make-grid (vec (apply map (comp vec reverse vector) shape-grid-data))))))

(defn collision? [grid shape]
  (let [padded-shape-grid (place-shape (make-grid) shape)
        cells-colide? (fn [c1 c2] (and (not= c1 :empty) (not= c2 :empty)))]
    (some (fn [x] x)
          (flatten (map (fn [row1 row2] 
                          (map cells-colide? row1 row2)) (:data grid) (:data padded-shape-grid))))))

(defn make-random-shape []
  (let [position [0 (- (/ (:cols grid-size) 2) 1)]
        shape (first (shuffle shapes))
        body (vec (for [row (:grid shape)] (vec (replace {\X (:color shape)
                                                          \_ :empty} row))))]
    (->Shape position (make-grid body))))
