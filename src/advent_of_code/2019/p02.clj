(ns advent-of-code.2019.p02
  (:require [clojure.test :refer [deftest testing is]]
            [advent-of-code.2019.intcode :as intcode]))

(def input "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,6,1,19,1,19,5,23,2,10,23,27,2,27,13,31,1,10,31,35,1,35,9,39,2,39,13,43,1,43,5,47,1,47,6,51,2,6,51,55,1,5,55,59,2,9,59,63,2,6,63,67,1,13,67,71,1,9,71,75,2,13,75,79,1,79,10,83,2,83,9,87,1,5,87,91,2,91,6,95,2,13,95,99,1,99,5,103,1,103,2,107,1,107,10,0,99,2,0,14,0")

(defn part-1 [input a b]
  (->> (intcode/program-state [] (intcode/parse-to-program input))
       (#(update % :program assoc 1 a))
       (#(update % :program assoc 2 b))
       (intcode/interpret-till intcode/wait-or-halt?)
       :program
       (#(get % 0))))

(defn part-2 [input]
  (->> (for [i (range 100) j (range 100)] [i j])
       (filter #(= 19690720 (apply part-1 input %)))
       (first)))

(deftest requirements
  (testing "requirements"
    (is (= 4570637 (part-1 input 12 2)))
    (is (= [54 85] (part-2 input)))))
