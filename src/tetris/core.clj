(ns tetris.core
  (:gen-class)
  (:require [tetris.grid :as grid])
  (:import [javax.swing JFrame JPanel]
           [java.awt.event KeyEvent]))

(def grid (atom nil))
(def shape (atom nil))

(defn make-panel []
  (proxy [JPanel] []
    (getPreferredSize [] grid/grid-dimension)
    (paintComponent [g]
      (grid/paint g (grid/place-shape @grid @shape)))))

(defn shape-update-col [shape f]
  (swap! shape (fn [old-shape]
                 (let [grid-width (:cols grid/grid-size)
                       shape-width (count (first (:body @shape)))
                       [row col] (:position old-shape)
                       new-col (f col)
                       in-bounds? (<= 0 new-col (- grid-width shape-width))]
                   (if in-bounds?
                     (assoc old-shape :position [row new-col])
                     old-shape)))))

(defn make-input-handler [panel]
  (proxy [java.awt.event.KeyListener] []
    (keyPressed [e]
      (let [key-code (.getKeyCode e)]
        (cond
          (= key-code KeyEvent/VK_LEFT) (shape-update-col shape dec)
          (= key-code KeyEvent/VK_RIGHT) (shape-update-col shape inc)))
      (.repaint panel))
    (keyReleased [e])
    (keyTyped [e])))

(defn make-frame []
  (let [panel (make-panel)]
    (doto (JFrame. "Clojure Tetris")
      (.add panel)
      (.pack)
      (.addKeyListener (make-input-handler panel))
      (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
      (.setVisible true))))

(defn game-reset! []
  (reset! grid (grid/make-grid))
  (reset! shape (grid/make-random-shape)))
  
(defn -main
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (game-reset!)
  (make-frame))
