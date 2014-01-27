(ns tetris.core
  (:gen-class)
  (:require [tetris.grid :as grid])
  (:import [javax.swing JFrame JPanel]
           [java.awt.event KeyEvent]))

(defn print-thread [s]
  (let [thread-name (-> (Thread/currentThread) (.getName))]
    (println thread-name s)))

(def shared-grid (atom nil))
(def shared-shape (atom nil))

(defn make-panel []
  (proxy [JPanel] []
    (getPreferredSize [] grid/grid-dimension)
    (paintComponent [g]
      (grid/paint g (grid/place-shape @shared-grid @shared-shape)))))

(defn shape-update-pos [shape grid row-fn col-fn]
  {:pre [(= (class shape) clojure.lang.Atom)]}
  (swap! shape (fn [old-shape]
                 (let [shape-grid (:grid @shape)
                       [row col] (:position old-shape)
                       new-pos [(row-fn row) (col-fn col)]
                       in-bounds? (and (<= 0 (new-pos 1) (- (grid/cols grid) (grid/cols shape-grid)))
                                       (<= (new-pos 0) (- (grid/rows grid) (grid/rows shape-grid))))]
                   (if in-bounds?
                     (assoc old-shape :position new-pos)
                     old-shape)))))

(defn shape-update-rotation [shape]
  {:pre [(= (class shape) clojure.lang.Atom)]}
  (swap! shape (fn [old-shape] (grid/rotate-shape old-shape))))

(defn make-input-handler [panel]
  (proxy [java.awt.event.KeyListener] []
    (keyPressed [e]
      (let [key-code (.getKeyCode e)]
        (cond
          (= key-code KeyEvent/VK_LEFT) (shape-update-pos shared-shape @shared-grid identity dec)
          (= key-code KeyEvent/VK_RIGHT) (shape-update-pos shared-shape @shared-grid identity inc)
          (= key-code KeyEvent/VK_UP) (shape-update-rotation shared-shape)))      
      (.repaint panel))
    (keyReleased [_])
    (keyTyped [_])))

(defn make-update-task [panel]
  (proxy [java.util.TimerTask] []
    (run []
      (shape-update-pos shared-shape @shared-grid inc identity)
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
  (reset! shared-grid (grid/make-grid))
  (reset! shared-shape (grid/make-random-shape)))
  
(defn -main
  [& args]
  ;; work around dangerous default behaviour in Clojure
  (alter-var-root #'*read-eval* (constantly false))
  (game-reset!)
  (make-frame))
