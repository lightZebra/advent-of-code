(ns advent-of-code.2019.p5
  (:require [clojure.string :as str]))

(def input "3,225,1,225,6,6,1100,1,238,225,104,0,1002,148,28,224,1001,224,-672,224,4,224,1002,223,8,223,101,3,224,224,1,224,223,223,1102,8,21,225,1102,13,10,225,1102,21,10,225,1102,6,14,225,1102,94,17,225,1,40,173,224,1001,224,-90,224,4,224,102,8,223,223,1001,224,4,224,1,224,223,223,2,35,44,224,101,-80,224,224,4,224,102,8,223,223,101,6,224,224,1,223,224,223,1101,26,94,224,101,-120,224,224,4,224,102,8,223,223,1001,224,7,224,1,224,223,223,1001,52,70,224,101,-87,224,224,4,224,1002,223,8,223,1001,224,2,224,1,223,224,223,1101,16,92,225,1101,59,24,225,102,83,48,224,101,-1162,224,224,4,224,102,8,223,223,101,4,224,224,1,223,224,223,1101,80,10,225,101,5,143,224,1001,224,-21,224,4,224,1002,223,8,223,1001,224,6,224,1,223,224,223,1102,94,67,224,101,-6298,224,224,4,224,102,8,223,223,1001,224,3,224,1,224,223,223,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,108,677,677,224,102,2,223,223,1005,224,329,101,1,223,223,1107,677,226,224,102,2,223,223,1006,224,344,101,1,223,223,1107,226,226,224,102,2,223,223,1006,224,359,101,1,223,223,1108,677,677,224,102,2,223,223,1005,224,374,101,1,223,223,8,677,226,224,1002,223,2,223,1005,224,389,101,1,223,223,108,226,677,224,1002,223,2,223,1006,224,404,1001,223,1,223,107,677,677,224,102,2,223,223,1006,224,419,101,1,223,223,1007,226,226,224,102,2,223,223,1005,224,434,101,1,223,223,1007,677,677,224,102,2,223,223,1005,224,449,1001,223,1,223,8,677,677,224,1002,223,2,223,1006,224,464,101,1,223,223,1108,677,226,224,1002,223,2,223,1005,224,479,101,1,223,223,7,677,226,224,1002,223,2,223,1005,224,494,101,1,223,223,1008,677,677,224,1002,223,2,223,1006,224,509,1001,223,1,223,1007,226,677,224,1002,223,2,223,1006,224,524,1001,223,1,223,107,226,226,224,1002,223,2,223,1006,224,539,1001,223,1,223,1107,226,677,224,102,2,223,223,1005,224,554,101,1,223,223,1108,226,677,224,102,2,223,223,1006,224,569,101,1,223,223,108,226,226,224,1002,223,2,223,1006,224,584,1001,223,1,223,7,226,226,224,1002,223,2,223,1006,224,599,101,1,223,223,8,226,677,224,102,2,223,223,1005,224,614,101,1,223,223,7,226,677,224,1002,223,2,223,1005,224,629,101,1,223,223,1008,226,677,224,1002,223,2,223,1006,224,644,101,1,223,223,107,226,677,224,1002,223,2,223,1005,224,659,1001,223,1,223,1008,226,226,224,1002,223,2,223,1006,224,674,1001,223,1,223,4,223,99,226")
(def test-input "1002,4,3,4,33")
(def test-input-2 "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,\n1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,\n999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99")

(defn parse-to-map [input]
  (->> (str/split input #",")
       (map (comp str/trim str/trim-newline))
       (map #(Integer/parseInt %))
       (map-indexed vector)
       (into (sorted-map))))

(defrecord ProgramState [position input output program])

(defn hide-program [state]
  (assoc state :program {:program :hidden}))

(defn arg [{:keys [position program]} pos]
  (get program (+ position 1 pos)))

(defn position [{:keys [program]} pos]
  (get program pos))

(defn instruction [{:keys [position program]}]
  (get program position))

(defn op-code [state]
  (mod (instruction state) 100))

(defn halt? [state]
  (= 99 (op-code state)))

(defn mode [instruction pos]
  (let [div        (apply * (repeat (+ 2 pos) 10))
        mode-value (mod (quot instruction div) 10)]
    (case mode-value
      0 :positional
      1 :immediate)))

(defn parameter [state pos]
  (let [parameter-value (arg state pos)]
    (case (mode (instruction state) pos)
      :positional (position state parameter-value)
      :immediate parameter-value)))

(defn parameters [state limit]
  (map (partial parameter state) (range limit)))

(defmulti interpret op-code)

(defn iterate-till-halt [state]
  (->> state
       (iterate interpret)
       ;(map #(doto % prn))
       (filter halt?)
       (first)))

(defmethod interpret 1 [state]
  (let [[a b] (parameters state 2)
        c (arg state 2)]
    (-> state
        (update :position (partial + 4))
        (update :program #(assoc % c (+ a b))))))

(defmethod interpret 2 [state]
  (let [[a b] (parameters state 2)
        c (arg state 2)]
    (-> state
        (update :position (partial + 4))
        (update :program #(assoc % c (* a b))))))

(defmethod interpret 3 [{:keys [input] :as state}]
  (let [a (arg state 0)]
    (-> state
        (update :position (partial + 2))
        (update :program #(assoc % a (first input)))
        (update :input rest))))

(defmethod interpret 4 [state]
  (let [[a] (parameters state 1)]
    (-> state
        (update :position (partial + 2))
        (update :output #(conj % a)))))

(defmethod interpret 5 [state]
  (let [[a b] (parameters state 2)
        position (if-not (zero? a) (constantly b) (partial + 3))]
    (update state :position position)))

(defmethod interpret 6 [state]
  (let [[a b] (parameters state 2)
        position (if (zero? a) (constantly b) (partial + 3))]
    (update state :position position)))

(defmethod interpret 7 [state]
  (let [[a b] (parameters state 2)
        c     (arg state 2)
        value (if (< a b) 1 0)]
    (-> state
        (update :position (partial + 4))
        (update :program #(assoc % c value)))))

(defmethod interpret 8 [state]
  (let [[a b] (parameters state 2)
        c     (arg state 2)
        value (if (= a b) 1 0)]
    (-> state
        (update :position (partial + 4))
        (update :program #(assoc % c value)))))

(defmethod interpret 99 [state] state)

(defn part [args io-input]
  (->> (parse-to-map args)
       (->ProgramState 0 io-input [])
       (iterate-till-halt)
       (hide-program)))

(defn -main [args]
  (->> (parse-to-map args)
       (->ProgramState 0 '(9) [])
       (iterate-till-halt)
       (hide-program)))

(comment
  (-main input)
  (-main test-input)
  (-main test-input-2)

  (part input '(1))
  (part input '(5)))
