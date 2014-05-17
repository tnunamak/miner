(ns miner.core
  (:require [lanterna.screen :as s])
  (:require [overtone.at-at :as at])
  (:require [clojure.math.numeric-tower :as math]))


(defn clear-screen [scr]
  (let [[cols rows] (vec (s/get-size scr))]
    (doseq [x (range cols)
            y (range rows)]
      (s/put-string scr x y " "))))

(defn get-cursor-coords [scr [ox oy]]
  (let [[cols rows] (s/get-size scr)]
  [(int (+ ox (/ cols 2))) (int (+ oy (/ rows 2)))]))

(defn valuable? [mineral]
  (case mineral
    (:platinum, :gold, :silver, :ruby) true
    false))

(defn collect-mineral [game]
  (let [cursor (get-cursor-coords (:screen game) (:origin game))
        mineral (or (get (:board game) cursor) :empty)
        mineral-in-cargo (mineral (:cargo game))]
    (println (:cargo game))
    (assoc-in
     (if (valuable? mineral)
       (assoc-in game [:cargo mineral] (if-not mineral-in-cargo 1 (inc mineral-in-cargo)))
       game)
     [:board cursor]
     :empty)))


(defn process-input [game]
  (let [[x y] (:origin game)
        key-pressed (s/get-key (:screen game))]
    (reduce (fn [game [key-in-game value]] ;; There has to be a better way to update multiple values in a map
              (assoc-in game [key-in-game] value))
            game
            [(let [new-origin (case key-pressed
                               :left [(- x 1) y]
                               :right [(+ x 1) y]
                               :up [x (- y 1)]
                               :down [x (+ y 1)]
                               [x y])]
              (if (or (:drill-active game)
                      (= (get (:board game) (get-cursor-coords (:screen game) new-origin)) :empty))
                [:origin new-origin]
                [:origin [x y]]))
             (case key-pressed
              \d [:drill-active true]
              (:left :right :up :down) [:drill-active false]
              [:drill-active (:drill-active game)])])))

(defn get-magnitude [vect]
  (math/sqrt (apply + (map #(math/expt % 2) vect))))

(defn choose-random-mineral [distance-traveled]
  (let [value (rand 2)
        boosted-value (+ value (/ distance-traveled 1000))]
    (cond
     (> value 1.0)   :empty
     (> boosted-value 0.999) :platinum
     (> boosted-value 0.990) :gold
     (> boosted-value 0.950) :silver
     (> boosted-value 0.900) :ruby
     :else :rock)))

(defn cell [game x y]
  (let [current-cell (get [x y] (:board game))]
    (if current-cell
      [[x y] current-cell]
      [[x y] (choose-random-mineral (get-magnitude (:origin game)))])))

(defn build-board [game]
  (assoc-in game [:board]
     (merge (apply hash-map ;; todo: figure how how to compose these applies
           (apply concat
                  (let [board (:board game)
                        [ox oy] (:origin game)
                        [cols rows] (s/get-size (:screen game))]
                    (for [x (range ox (+ ox cols))
                          y (range oy (+ oy rows))]
                      (cell game x y)))))
            (:board game))))

(defn draw-board [game]
  (doseq [[[x y] value] (:board game)]
    (let [[ox oy] (:origin game)
          color (case value
                  :rock :green
                  :platinum :blue
                  :gold :yellow
                  :silver :magenta
                  :ruby :red
                  :default)]
      (s/put-string (:screen game) (- x ox) (- y oy) " " {:bg color}))))

(defn tick [my-pool game]
  (let [new-game (build-board (collect-mineral (process-input game)))]
    (draw-board new-game)
    (s/redraw (:screen new-game))
    (at/at (+ 10 (at/now)) #(tick my-pool new-game) my-pool)))

(defn bootstrap []
  (def my-pool (at/mk-pool))

  (def scr (s/get-screen))
  (s/start scr)
  (let [[cols rows] (s/get-size scr)]
    (s/move-cursor scr (int (/ cols 2)) (int (/ rows 2))))

  (tick my-pool {:screen scr
                 :board {}
                 :origin [0 0]
                 :cargo {:platinum 0
                         :gold 0
                         :silver 0
                         :ruby 0}}))

(defn -main[] (bootstrap))
