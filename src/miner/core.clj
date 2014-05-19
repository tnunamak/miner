(ns miner.core
  (:gen-class :main true)
  (:require [lanterna.screen :as s])
  (:require [overtone.at-at :as at])
  (:require [clojure.math.numeric-tower :as math]))

(defn get-cursor-coords
  ([game]
   (get-cursor-coords (:screen game) (:origin game)))
  ([scr [ox oy]]
   (let [[cols rows] (s/get-size scr)]
     [(int (+ ox (/ cols 2))) (int (+ oy (/ rows 2)))])))

(defn valuable? [mineral]
  (case mineral
    (:platinum, :gold, :silver, :ruby) true
    false))

(defn fuel? [mineral]
  (= :fuel mineral))

(defn shop? [mineral]
  (= :shop mineral))

(defn mineral-value-of [mineral]
  (case mineral
    :platinum 50
    :gold 10
    :silver 3
    :ruby 1
    0))

(defn count-cargo [game]
  (apply + (vals (:cargo game))))

(defn cash-for-cargo [game]
  (assoc-in game [:money] (+ (:money game)
                             (reduce
                              #(+ %1 (* (mineral-value-of (key %2)) (val %2)))
                              0 (:cargo game)))))

(defn sell-cargo [game]
  (assoc-in (cash-for-cargo game) [:cargo] {:platinum 0
                                            :gold 0
                                            :silver 0
                                            :ruby 0}))

(defn empty-at-cursor [game]
  (assoc-in
   game
   [:board (get-cursor-coords game)]
   :empty))

(defn count-mineral [game mineral]
  (if (< (count-cargo game) (:cargo-capacity game))
    (let [mineral-in-cargo (mineral (:cargo game))]
      (assoc-in game [:cargo mineral] (if-not mineral-in-cargo 1 (inc mineral-in-cargo))))
    game))

(defn collect-mineral [game]
  (let [mineral (or (get (:board game) (get-cursor-coords game)) :empty)]
    (cond
     (fuel? mineral) (assoc-in game [:fuel] (:fuel-capacity game))
     (shop? mineral) (sell-cargo game)
     (valuable? mineral) (empty-at-cursor (count-mineral game mineral))
     :else (empty-at-cursor game))))

(defn process-input [game]
  (let [[x y] (:origin game)
        key-pressed (s/get-key (:screen game))]
    (reduce (fn [game [key-in-game value]] ;; There has to be a better way to update multiple values in a map
              (assoc-in game [key-in-game] value))
            game
            (let [new-origin (case key-pressed
                               :left [(- x 1) y]
                               :right [(+ x 1) y]
                               :up [x (- y 1)]
                               :down [x (+ y 1)]
                               [x y])
                  mineral-type (get (:board game) (get-cursor-coords (:screen game) new-origin))
                  is-move (and (not= new-origin [x y])
                               (or (:drill-active game)
                                   (= mineral-type :empty)
                                   (= mineral-type :fuel)
                                   (= mineral-type :shop)))]
              [(if is-move
                 [:fuel (- (:fuel game) (if (:drill-active game) 3 1))])
               (if is-move
                 [:origin new-origin]
                 [:origin [x y]])
               (case key-pressed
                 \d [:drill-active (not (:drill-active game))]
                 [:drill-active (:drill-active game)])]))))

(defn get-magnitude [vect]
  (math/sqrt (apply + (map #(math/expt % 2) vect))))

(defn choose-random-mineral [distance-traveled]
  (let [value (rand 2)
        boosted-value (+ value (/ distance-traveled 1000))]
    (cond
     (> value 1.999)  :fuel
     (> value 1.998)  :shop
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
                  (:fuel :shop) :magenta
                  :rock :green
                  :platinum :blue
                  :gold :yellow
                  :silver :cyan
                  :ruby :red
                  :default)
          label (case value
                  :fuel "F"
                  :shop "S"
                  " ")]
      (s/put-string (:screen game) (- x ox) (- y oy) label {:bg color}))))

(defn draw-hud [game]
  (s/put-string (:screen game) 0 0 (str
                                    "Cargo: " "(" (count-cargo game) "/" (:cargo-capacity game) ")" (:cargo game) "\t"
                                    "Drill (d): " (if (:drill-active game) "on" "off") "\t"
                                    "Fuel: " (:fuel game) "\t"
                                    "$" (:money game)
                                    "\t\t\t\t\t")))

(defn tick [my-pool game]
  (let [new-game (build-board (collect-mineral (process-input game)))]
    (draw-board new-game)
    (draw-hud new-game)
    (s/redraw (:screen new-game))
    (if (> (:fuel new-game) 0)
      (at/at (+ 10 (at/now)) #(tick my-pool new-game) my-pool)
      (do
        (s/put-string (:screen new-game) 0 1 "GAME OVER\t\t\t\t\t\t\t\t\t\t\t\t")
        (s/redraw (:screen new-game))))))

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
                         :ruby 0}
                 :cargo-capacity 10
                 :money 0
                 :fuel 100
                 :fuel-capacity 100}))

(defn -main[] (bootstrap))
