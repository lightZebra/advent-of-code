(ns advent-of-code.2019.intcode
  (:require [clojure.string :as str]))

(defrecord ProgramState [position relative-base input output program])

(defn program-state [input program]
  (->ProgramState 0 0 input [] program))

(defn hide-program [state]
  (assoc state :program {:program :hidden}))

(defn arg [{:keys [position program]} pos]
  (get program (+ position 1 pos) 0))

(defn position [{:keys [program]} pos]
  (get program pos 0))

(defn relative-position [{:keys [relative-base program]} pos]
  (get program (+ relative-base pos) 0))

(defn instruction [{:keys [position program]}]
  (get program position))

(defn op-code [state]
  (mod (instruction state) 100))

(defn halt? [state]
  (= 99 (op-code state)))

(defn input-wait? [{:keys [input] :as state}]
  (and (= 3 (op-code state))
       (empty? input)))

(def wait-or-halt? (some-fn input-wait? halt?))

(defn assoc-input [state input]
  (assoc state :input input))

(defn append-inputs [state inputs]
  (update state :input #(concat % inputs)))

(defn clear-output [state]
  (assoc state :output []))

(defn mode [instruction pos]
  (let [div (int (Math/pow 10 (+ 2 pos)))]
    (case (mod (quot instruction div) 10)
      0 :positional
      1 :immediate
      2 :relative)))

(defn parameter [state pos]
  (let [parameter-value (arg state pos)]
    (case (mode (instruction state) pos)
      :positional (position state parameter-value)
      :immediate parameter-value
      :relative (relative-position state parameter-value))))

(defn parameter-to [state pos]
  (let [parameter-value (arg state pos)]
    (case (mode (instruction state) pos)
      :positional parameter-value
      :relative (+ (:relative-base state) parameter-value))))

(defn parameters [state limit]
  (map (partial parameter state) (range limit)))

(defmulti interpret op-code)

(defmethod interpret 1 [state]
  (let [[a b] (parameters state 2)
        c (parameter-to state 2)]
    (-> state
        (update :position + 4)
        (update :program #(assoc % c (+ a b))))))

(defmethod interpret 2 [state]
  (let [[a b] (parameters state 2)
        c (parameter-to state 2)]
    (-> state
        (update :position + 4)
        (update :program #(assoc % c (* a b))))))

(defmethod interpret 3 [{:keys [input] :as state}]
  (let [a (parameter-to state 0)]
    (-> state
        (update :position + 2)
        (update :program #(assoc % a (first input)))
        (update :input rest))))

(defmethod interpret 4 [state]
  (let [[a] (parameters state 1)]
    (-> state
        (update :position + 2)
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
        c     (parameter-to state 2)
        value (if (< a b) 1 0)]
    (-> state
        (update :position + 4)
        (update :program #(assoc % c value)))))

(defmethod interpret 8 [state]
  (let [[a b] (parameters state 2)
        c     (parameter-to state 2)
        value (if (= a b) 1 0)]
    (-> state
        (update :position + 4)
        (update :program #(assoc % c value)))))

(defmethod interpret 9 [state]
  (let [[a] (parameters state 2)]
    (-> state
        (update :position + 2)
        (update :relative-base + a))))

(defmethod interpret 99 [state] state)

(defn interpret-till
  ([pred state]
   (interpret-till interpret pred state))
  ([f pred state]
   (first (filter pred (iterate f state)))))

(defn parse-to-program [input]
  (->> (str/split input #",")
       (map (comp str/trim str/trim-newline))
       (map #(Long/parseLong %))
       (map-indexed vector)
       (into (sorted-map))))
