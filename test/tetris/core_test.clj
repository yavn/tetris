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
        rotated-grid (rotate-grid grid)]
    (is (= grid
           [[:red :red :empty]
            [:empty :red :red]]))
    (is (= rotated-grid
           [[:empty :red]
            [:red :red]
            [:red :empty]]))
    (is (= grid
           (-> grid rotate-grid rotate-grid rotate-grid rotate-grid))
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
        game-grid (make-grid 1 1)
        game-grid-with-block (place-block-in-grid game-grid block)]
    (is (= game-grid-with-block
           [[:blue]]))))

(deftest test-collision-when-placing-a-block
  (let [block-grid [[:red :red]
                    [:red :red]]
        game-grid (make-grid 2 3)]
    (is (= (place-block-in-grid game-grid (make-block [0 0] block-grid))
           [[:red :red :empty]
            [:red :red :empty]])
        "A small block should fit comfortably in an empty grid.")))

(deftest test-crop-grid
  (let [grid [[1 2 3]
              [4 5 6]
              [7 8 9]]]
    (is (= (crop-grid grid 0 0)
           grid)
        "Nothing cropped, should yield the same grid.")
    (is (= (crop-grid grid 1 0)
           [[4 5 6]
            [7 8 9]])
        "Should drop the first row.")
    (is (= (crop-grid grid 0 1)
           [[2 3]
            [5 6]
            [8 9]])
        "Should drop the first column.")
    (is (= (crop-grid grid 2 2)
           [[9]])
        "Should drop everything but the lower-right element.")))
        