(ns tetris.grid-test
  (:use clojure.test
        tetris.grid))

(deftest test-make-grid-with-size
  (testing "Grid constructor and properties"
           (let [grid (make-grid 3 2)]
             (is (= (class grid) tetris.grid.Grid))
             (is (= (.rows grid) 3))
             (is (= (.cols grid) 2))
             (is (every? #(= % :empty) (flatten (:data grid)))))))

(deftest test-make-shape
  (testing "Shape properties"
           (let [shape (make-random-shape)]
             (is (= (class shape) tetris.grid.Shape))
             (is (= (class (:body shape)) tetris.grid.Grid))
             (is (vector? (:position shape)))
             (is (= (count (:position shape)) 2)))))

(deftest test-place-shape
  (testing "Placing a shape in a grid"
           (let [shape-body (make-grid [[:red :empty]
                                        [:empty :red]])
                 shape (->Shape [0 1] shape-body)
                 grid (make-grid 3 3)
                 grid-with-shape (place-shape grid shape)]
             (is (= (:data grid-with-shape)
                    [[:empty :red   :empty]
                     [:empty :empty :red]
                     [:empty :empty :empty]])))))

(deftest test-rotate-shape
  (testing "Rotating a shape"
           (let [shape-body (make-grid [[:red :empty]
                                        [:red :red]])
                 shape (->Shape [0 0] shape-body)
                 rotated-shape (rotate-shape shape)]
             (is (= (:data (:body rotated-shape))
                    [[:red :red]
                     [:red :empty]])))))
