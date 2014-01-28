(ns tetris.core
  (:gen-class)
  (:import [javax.swing JFrame JPanel]
           [java.awt.event KeyEvent]))

(def grid-size { :rows 14 :cols 10 })
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
  
(defn -main
 [& args]
 ;; work around dangerous default behaviour in Clojure
 (alter-var-root #'*read-eval* (constantly false)))
