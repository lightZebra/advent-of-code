(ns advent-of-code.2021.p6
  (:require [clojure.string :as str]))

(def example "3,4,3,1,2")
(def input "2,1,2,1,5,1,5,1,2,2,1,1,5,1,4,4,4,3,1,2,2,3,4,1,1,5,1,1,4,2,5,5,5,1,1,4,5,4,1,1,4,2,1,4,1,2,2,5,1,1,5,1,1,3,4,4,1,2,3,1,5,5,4,1,4,1,2,1,5,1,1,1,3,4,1,1,5,1,5,1,1,5,1,1,4,3,2,4,1,4,1,5,3,3,1,5,1,3,1,1,4,1,4,5,2,3,1,1,1,1,3,1,2,1,5,1,1,5,1,1,1,1,4,1,4,3,1,5,1,1,5,4,4,2,1,4,5,1,1,3,3,1,1,4,2,5,5,2,4,1,4,5,4,5,3,1,4,1,5,2,4,5,3,1,3,2,4,5,4,4,1,5,1,5,1,2,2,1,4,1,1,4,2,2,2,4,1,1,5,3,1,1,5,4,4,1,5,1,3,1,3,2,2,1,1,4,1,4,1,2,2,1,1,3,5,1,2,1,3,1,4,5,1,3,4,1,1,1,1,4,3,3,4,5,1,1,1,1,1,2,4,5,3,4,2,1,1,1,3,3,1,4,1,1,4,2,1,5,1,1,2,3,4,2,5,1,1,1,5,1,1,4,1,2,4,1,1,2,4,3,4,2,3,1,1,2,1,5,4,2,3,5,1,2,3,1,2,2,1,4")

(defn parse [input]
  (->> (str/split input #",")
       (map #(Integer/parseInt %))))

(defn next-day [state]
  (reduce
   (fn [acc timer]
     (if (zero? timer)
       (conj acc 6 8)
       (conj acc (dec timer))))
   []
   state))

(defn part-1 [input]
  (->> input
       (iterate next-day)
       (drop 80)
       (first)
       (count)))

(def fish-count
  (memoize
   (fn [timer days]
     (if (< days 0)
       0
       (->> (range (- days timer 1) 0 -7)
            (map #(fish-count 8 %))
            (reduce + 1))))))

(defn part-2 [input]
  (reduce + 0 (map #(fish-count % 257) input)))

(comment
  (-> example parse part-1)
  (-> input parse part-1)
  (-> example parse part-2)
  (-> input parse part-2))
