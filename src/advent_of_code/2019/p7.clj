(ns advent-of-code.2019.p7
  (:require [clojure.test :refer [deftest testing is]]
            [advent-of-code.2019.intcode :as intcode]))

(def input "3,8,1001,8,10,8,105,1,0,0,21,38,55,72,93,118,199,280,361,442,99999,3,9,1001,9,2,9,1002,9,5,9,101,4,9,9,4,9,99,3,9,1002,9,3,9,1001,9,5,9,1002,9,4,9,4,9,99,3,9,101,4,9,9,1002,9,3,9,1001,9,4,9,4,9,99,3,9,1002,9,4,9,1001,9,4,9,102,5,9,9,1001,9,4,9,4,9,99,3,9,101,3,9,9,1002,9,3,9,1001,9,3,9,102,5,9,9,101,4,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,99,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,99,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,99")
(def test-input "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,\n-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,\n53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10")

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
  (loop [amplifiers amplifiers
         input      input
         index      0]
    (if (= index (count amplifiers))
      [input amplifiers]
      (let [amplifier  (intcode/append-inputs (get amplifiers index) input)
            amplifier  (intcode/interpret-till intcode/wait-or-halt amplifier)
            {:keys [output]} amplifier
            amplifiers (assoc amplifiers index (intcode/clear-output amplifier))]
        (recur amplifiers output (inc index))))))

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

(defn -main [args]
  (let [code       (intcode/parse-to-program args)
        amplifiers [(intcode/->ProgramState 0 0 [9] [] code)
                    (intcode/->ProgramState 0 0 [7] [] code)
                    (intcode/->ProgramState 0 0 [8] [] code)
                    (intcode/->ProgramState 0 0 [5] [] code)
                    (intcode/->ProgramState 0 0 [6] [] code)]]
    (interpret-loop [0] amplifiers)))

(deftest requirements
  (testing "requirements"
    (is (= 368584 (part-1 input)))
    (is (= 35993240 (part-2 input)))))

(comment
  (-main test-input)

  (part-1 input)
  (part-2 input))
