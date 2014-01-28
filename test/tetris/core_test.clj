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
            [:red :empty]]))))


