(ns tetris.grid)

(def ^:private grid-size { :rows 14 :cols 10 })
(def ^:private cell-size-in-pixels 32)

(defn- cells->pixels [n]
  (* n cell-size-in-pixels))

(def grid-dimension
  (java.awt.Dimension. (cells->pixels (:cols grid-size))
                       (cells->pixels (:rows grid-size))))

(def ^:private colors 
  { :empty java.awt.Color/lightGray 
    :red java.awt.Color/red
    :blue java.awt.Color/blue
    :yellow java.awt.Color/yellow
    :green java.awt.Color/green
    :cyan java.awt.Color/cyan
    :magenta java.awt.Color/magenta })

(defprotocol GridProtocol
  (rows [this])
  (cols [this]))

(defrecord Grid [data]
  GridProtocol
  (rows [_] (count data))
  (cols [_] (count (first data))))

(defrecord Shape [position body])

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

(defmacro ^:private iterate-over-grid 
  "Repeatedly executes body for each cell in the grid binding row and cell
  indices and cell value to the specified symbols. E.g.:

  (iterate-over-grid grid row-index col-index cell
    (println \"Cell at [\" row-index col-index \"] has value\" cell))"
  
  [grid row col cell & body]
  `(let [height# (count ~grid)
         width# (count (first ~grid))]
     (doseq [~row (range height#) ~col (range width#)]
       (let [~cell (get-in ~grid [~row ~col])]
         ~@body))))

(defn paint [g grid]
  (iterate-over-grid grid row col color-name
                     (let [x (cells->pixels col)
                           y (cells->pixels row)]
                       (.setColor g (get colors color-name))
                       (.fillRect g x y cell-size-in-pixels cell-size-in-pixels))))

(defn place-shape [grid shape]
  (with-local-vars [new-grid grid]
    (iterate-over-grid (:body shape) row col color-name
                       (let [[pos-row pos-col] (:position shape)
                             grid-pos [(+ row pos-row) (+ col pos-col)]]
                         (when (not= color-name :empty)
                           (var-set new-grid
                                 (assoc-in @new-grid grid-pos color-name)))))
    @new-grid))

(defn collision? [grid shape]
  (let [padded-shape (place-shape (make-grid) shape)
        cells-colide? (fn [c1 c2] (and (not= c1 :empty) (not= c2 :empty)))]
    (some (fn [x] x)
          (flatten (map (fn [row1 row2] 
                          (map cells-colide? row1 row2)) grid padded-shape)))))

(defn make-random-shape []
  (let [X :red e :empty]
    (->Shape [0 (/ (:cols grid-size) 2)]
             [[X X X]
              [e X e]])))
