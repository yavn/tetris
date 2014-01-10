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

(defn shape-update-pos [shape row-fn col-fn]
  {:pre [(= (class shape) clojure.lang.Atom)]}
  (swap! shape (fn [old-shape]
                 (let [shape-height (count (:body @shape))
                       shape-width (count (first (:body @shape)))
                       [row col] (:position old-shape)
                       new-pos [(row-fn row) (col-fn col)]
                       in-bounds? (and (<= 0 (new-pos 1) (- (:cols grid/grid-size) shape-width))
                                       (<= (new-pos 0) (- (:rows grid/grid-size) shape-height)))]
                   (if in-bounds?
                     (assoc old-shape :position new-pos)
                     old-shape)))))

(defn make-input-handler [panel]
  (proxy [java.awt.event.KeyListener] []
    (keyPressed [e]
      (let [key-code (.getKeyCode e)]
        (cond
          (= key-code KeyEvent/VK_LEFT) (shape-update-pos shape identity dec)
          (= key-code KeyEvent/VK_RIGHT) (shape-update-pos shape identity inc)))
      (.repaint panel))
    (keyReleased [_])
    (keyTyped [_])))

(defn make-update-task [panel]
  (proxy [java.util.TimerTask] []
    (run []
      (shape-update-pos shape inc identity)
      (.repaint panel))))

(defn make-frame []
  (let [panel (make-panel)
        timer (java.util.Timer.)]
    (doto (JFrame. "Clojure Tetris")
      (.add panel)
      (.pack)
      (.addKeyListener (make-input-handler panel))
      (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
      (.addWindowListener
        (proxy [java.awt.event.WindowListener] []
          (windowActivated [_])
          (windowClosed [_]
            (.cancel timer))
          (windowClosing [_])
          (windowDeactivated [_])
          (windowDeiconified [_])
          (windowIconified [_])
          (windowOpened [_])))
      (.setVisible true))
    (.scheduleAtFixedRate timer (make-update-task panel) 1000 1000)))

(defn game-reset! []
  (reset! grid (grid/make-grid))
  (reset! shape (grid/make-random-shape)))
  
(defn -main
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (game-reset!)
  (make-frame))
