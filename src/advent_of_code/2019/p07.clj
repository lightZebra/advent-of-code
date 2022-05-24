(ns advent-of-code.2019.p07
  (:require [clojure.test :refer [deftest testing is]]
            [advent-of-code.2019.intcode :as intcode]))

(def input "3,8,1001,8,10,8,105,1,0,0,21,38,55,72,93,118,199,280,361,442,99999,3,9,1001,9,2,9,1002,9,5,9,101,4,9,9,4,9,99,3,9,1002,9,3,9,1001,9,5,9,1002,9,4,9,4,9,99,3,9,101,4,9,9,1002,9,3,9,1001,9,4,9,4,9,99,3,9,1002,9,4,9,1001,9,4,9,102,5,9,9,1001,9,4,9,4,9,99,3,9,101,3,9,9,1002,9,3,9,1001,9,3,9,102,5,9,9,101,4,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,99,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,99")

(defrecord AmplifiersState [input amplifiers])

(defn amplifier-permutations [start end]
  (for [a (range start end)
        b (range start end)
        c (range start end)
        d (range start end)
        e (range start end)
        :when (= 5 (count (distinct [a b c d e])))]
    [a b c d e]))

(defn indexes->amplifiers [program indexes]
  (mapv #(intcode/program-state (vector %) program) indexes))

(defn interpret [input amplifiers]
  (reduce
   (fn [[input acc] amplifier]
     (->> (intcode/append-inputs amplifier input)
          (intcode/interpret-till intcode/wait-or-halt?)
          ((juxt :output (comp #(conj acc %) intcode/clear-output)))))
   [input []]
   amplifiers))

(defn interpret-loop [input amplifiers]
  (->> (iterate (partial apply interpret) [input amplifiers])
       (filter (comp intcode/halt? first second))
       (first)))

(defn part-1 [args]
  (->> (amplifier-permutations 0 5)
       (map (partial indexes->amplifiers (intcode/parse-to-program args)))
       (map (partial interpret [0]))
       (map ffirst)
       (reduce max)))

(defn part-2 [args]
  (->> (amplifier-permutations 5 10)
       (map (partial indexes->amplifiers (intcode/parse-to-program args)))
       (map (partial interpret-loop [0]))
       (map ffirst)
       (reduce max)))

(deftest requirements
  (testing "requirements"
    (is (= 368584 (part-1 input)))
    (is (= 35993240 (part-2 input)))))
