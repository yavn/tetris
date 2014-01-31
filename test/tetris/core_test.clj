(ns tetris.core-test
  (:use clojure.test
        tetris.core :reload))

(def test-shape
  {:color :red
   :grid ["_XX"
          "XX_"]})

(deftest test-create-grid
  (let [grid (make-grid 2 3)]
    (is (= (grid-rows grid) 2))
    (is (= (grid-cols grid) 3))
    (is (= grid [[:empty :empty :empty]
                 [:empty :empty :empty]]))))

(deftest test-grid-create-with-shape
  (let [shape {:color :red
               :grid ["X_X"]}
        grid (make-grid-with-shape shape)]
    (is (= grid [[:red :empty :red]]))))

(deftest test-rotate-grid
  (let [shape {:color :red
               :grid ["XX_"
                      "_XX"]}
        grid (make-grid-with-shape shape)
        rotated-grid (grid-rotate grid)]
    (is (= grid
           [[:red :red :empty]
            [:empty :red :red]]))
    (is (= rotated-grid
           [[:empty :red]
            [:red :red]
            [:red :empty]]))
    (is (= grid
           (-> grid grid-rotate grid-rotate grid-rotate grid-rotate))
        "We expect a 360 deg rotation to yield the original grid.")))

(deftest test-create-block
  ;; Read up on destructuring (bindings in let and function arguments)
  ;; here: http://clojure.org/special_forms#binding-forms
  (let [block (make-block [1 2] [[:red]])
        {:keys [position grid]} block]
    (is (= position [1 2]))
    (is (= grid [[:red]]))))

(deftest test-place-block-in-grid
  (let [block-grid (make-grid-with-shape {:color :blue
                                          :grid ["X"]})
        block (make-block [0 0] block-grid)
        game-grid (make-grid 1 1)]
    (is (= (place-block-in-grid game-grid block)
           [[:blue]]))))

(deftest test-collision-when-placing-a-block
  (let [block-grid [[:red :red]
                    [:red :red]]
        game-grid [[:empty :empty :blue]
                   [:empty :empty :empty]]]
    (is (= (place-block-in-grid game-grid (make-block [0 0] block-grid))
           [[:red :red :blue]
            [:red :red :empty]])
        "Block should fit in the grid.")
    (is (= (place-block-in-grid game-grid (make-block [0 1] block-grid))
           nil)
        "There's not enough room to place the block.")
    (is (= (place-block-in-grid game-grid (make-block [1 0] block-grid))
           nil)
        "Block is outside the bounds of the grid.")))

(deftest test-indexed
  (is (= (indexed [:a :b])
         [[0 :a] [1 :b]]))
  (is (= (indexed [[:a :b][:c :d]])
         [[0 [:a :b]][1 [:c :d]]])))

(deftest test-get-in-grid
  (is (= (get-in-grid [[1 2 3][4 5 6]] [1 2])
         6))
  (is (= (get-in-grid [[1]] [1 0])
         :empty))
  (is (= (get-in-grid [[1]] [-1 0])
         :empty)))

(deftest test-place-cell
  (is (= (place-cell :empty :empty)
         :empty))
  (is (= (place-cell :red :empty)
         :red))
  (is (= (place-cell :empty :red)
         :red))
  (is (= (place-cell :red :blue)
         :collision)))

(deftest test-is-block-in-bounds
  (let [grid [[:empty]]
        block-grid [[:red]]]
    (is (true? (block-in-bounds? grid (make-block [0 0] block-grid))))
    (is (false? (block-in-bounds? grid (make-block [-1 0] block-grid))))
    (is (false? (block-in-bounds? grid (make-block [0 -1] block-grid))))
    (is (false? (block-in-bounds? grid (make-block [1 0] block-grid))))
    (is (false? (block-in-bounds? grid (make-block [0 1] block-grid))))))

(deftest test-move-block
  (let [block (make-block [1 1] [[:red]])]
    (is (= (:position (block-move block [0 -1]))
           [1 0]))
    (is (= (:position (block-move block [0 1]))
           [1 2]))))

(deftest test-update-block
  ;; This function is a bit complicated but the contract is fairly simple:
  ;; Do some kind of an update on the block and return the new block.
  ;; If the new block is not valid (e.g. collides with grid or is out of bounds
  ;; return the old (unchanged) block instead.
  (let [block (make-block [0 0] [[:red :red]])
        grid (make-grid 2 2)]
    (is (= (update-block block block-rotate grid)
           (make-block [0 0] [[:red]
                              [:red]]))
        "There's enough room, so it should be possible to rotate the block.")
    (is (= (update-block block block-move grid [0 1])
           block)
        "Moving block to the right moves it off bounds, so this should fail i.e. yield the original block.")))
       