(ns advent-of-code.2021.p11
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def example "5483143223\n2745854711\n5264556173\n6141336146\n6357385478\n4167524645\n2176841721\n6882881134\n4846848554\n5283751526")
(def input "5421451741\n3877321568\n7583273864\n3451717778\n2651615156\n6377167526\n5182852831\n4766856676\n3437187583\n3633371586")

(defn parse [input]
  (->> (str/split-lines input)
       (map seq)
       (mapv (partial mapv #(Integer/parseInt (str %))))))

(def adjacent-points-delta
  [[0 1]
   [1 0]
   [0 -1]
   [-1 0]
   [1 1]
   [1 -1]
   [-1 1]
   [-1 -1]])

(defn adjacent-points [input point]
  (let [len-i (dec (count input))
        len-j (dec (count (first input)))]
    (->> (map (partial map + point) adjacent-points-delta)
         (filter (fn [[i j]]
                   (and (<= 0 i len-i)
                        (<= 0 j len-j)))))))

(defn increment-every [input]
  (mapv (partial mapv inc) input))

(defn charged-points [input]
  (->> (for [i (range (count input))
             j (range (count (first input)))]
         (when (<= 10 (nth (nth input i) j))
           [i j]))
       (filter some?)
       (set)))

(defn increment-adjacent [input charged]
  (->> (mapcat (partial adjacent-points input) charged)
       (reduce #(update-in %1 %2 inc) input)))

(defn update-blinked [input blinked]
  (reduce (fn [input point] (assoc-in input point 0)) input blinked))

(defn step [input]
  (loop [input   (increment-every input)
         blinked #{}]
    (let [charged (set/difference (charged-points input) blinked)]
      (if (empty? charged)
        (update-blinked input blinked)
        (recur (increment-adjacent input charged)
               (set/union blinked charged))))))

(defn count-zero [input]
  (count (mapcat (partial filter #{0}) input)))

(defn part-1 [input]
  (->> (iterate step input)
       (take 101)
       (map count-zero)
       (reduce + 0)))

(defn part-2 [input]
  (->> (map-indexed vector (iterate step input))
       (drop-while (fn [[_ input]] (not-every? #{0} (flatten input))))
       (take 1)
       (ffirst)))

(comment
  (-> example parse part-1)
  (-> input parse part-1)
  (-> example parse part-2)
  (-> input parse part-2))
