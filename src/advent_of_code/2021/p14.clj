(ns advent-of-code.2021.p14
  (:require [clojure.string :as str]))

(def example "NNCB\n\nCH -> B\nHH -> N\nCB -> H\nNH -> C\nHB -> C\nHC -> B\nHN -> C\nNN -> C\nBH -> H\nNC -> B\nNB -> B\nBN -> B\nBB -> N\nBC -> B\nCC -> N\nCN -> C")
(def input "CKKOHNSBPCPCHVNKHFFK\n\nKO -> C\nSO -> S\nBF -> V\nVN -> B\nOV -> K\nVH -> O\nKV -> N\nKB -> F\nNB -> C\nHS -> K\nPF -> B\nHB -> N\nOC -> H\nFS -> F\nVV -> S\nKF -> C\nFN -> F\nKP -> S\nHO -> N\nNH -> K\nOO -> S\nFB -> C\nBP -> F\nCH -> N\nSN -> O\nKN -> B\nCV -> O\nCC -> B\nVB -> C\nPH -> V\nCO -> K\nKS -> K\nBK -> N\nFH -> S\nPV -> H\nCB -> P\nFO -> F\nBB -> K\nOB -> C\nHH -> F\nON -> O\nFK -> B\nNF -> F\nSV -> F\nCP -> H\nSS -> B\nOP -> H\nNS -> O\nHK -> N\nBC -> P\nNV -> V\nVS -> F\nPC -> V\nCS -> F\nNP -> V\nPS -> F\nVC -> F\nKK -> S\nPO -> P\nHF -> H\nKC -> P\nSF -> N\nBV -> N\nFF -> V\nFV -> V\nBO -> N\nOS -> C\nOF -> H\nCN -> S\nNO -> O\nNC -> B\nVK -> C\nHN -> B\nPK -> N\nSK -> S\nHV -> F\nBH -> B\nOK -> S\nVO -> B\nBS -> H\nPP -> N\nSC -> K\nBN -> P\nFC -> S\nSB -> B\nSH -> H\nNN -> V\nNK -> N\nVF -> H\nCF -> F\nPB -> C\nSP -> P\nKH -> C\nVP -> N\nCK -> H\nHP -> P\nFP -> B\nHC -> O\nPN -> F\nOH -> H")

(defn parse [input]
  (let [[template & insertions] (filter seq (str/split-lines input))]
    {:template   (vec template)
     :insertions (->> insertions
                      (map #(str/split % #" -> "))
                      (map #(update % 0 vec))
                      (map #(update % 1 (comp first vec)))
                      (into {}))}))

(defn next-generation [state]
  (assoc state
         :template
         (->> (partition 2 1 (:template state))
              (reduce (fn [acc pair]
                        (conj acc
                              (get (:insertions state) pair)
                              (second pair)))
                      [(first (:template state))]))))

(defn part-1 [input]
  (->> (iterate next-generation input)
       (drop 10)
       (first)
       :template
       (frequencies)
       (map val)
       ((juxt (partial reduce max) (partial reduce min)))
       (apply -)))

(defn next-generation-with-counts [state]
  (letfn [(update-counts [counts [pair number]]
            (if-let [middle-ch (get (:insertions state) pair)]
              (update counts middle-ch (fnil + 0) number)
              counts))
          (update-template [acc [pair number]]
            (if-let [middle-ch (get (:insertions state) pair)]
              (->> (interpose middle-ch pair)
                   (partition 2 1)
                   (reduce (fn [acc pair] (update acc pair (fnil + 0) number)) acc))
              (update acc pair (fnil + 0) number)))]
    (-> state
        (update :counts #(reduce update-counts % (:template state)))
        (update :template #(reduce update-template {} %)))))

(defn part-2-bottom-up [input]
  (->> (assoc input
              :template (frequencies (partition 2 1 (:template input)))
              :counts (frequencies (:template input)))
       (iterate next-generation-with-counts)
       (drop 40)
       (first)
       :counts
       (map val)
       ((juxt (partial reduce max) (partial reduce min)))
       (reduce -)))

(defn dec-middle-char [middle-char [f & r]]
  (cons (update f middle-char dec) r))

(defn join-frequencies [a b]
  (reduce (fn [acc [char number]]
            (update acc char (fnil + 0) number))
          a b))

(def count-pairs-top-down
  (memoize
   (fn [insertions step template]
     (if (zero? step)
       (frequencies template)
       (->> (partition 2 1 template)
            (map (fn [pair]
                   (if-let [middle-ch (get insertions pair)]
                     (->> (interpose middle-ch pair)
                          (partition 2 1)
                          (map (partial count-pairs-top-down insertions (dec step)))
                          (dec-middle-char middle-ch))
                     (count-pairs-top-down insertions (dec step) pair))))
            (mapcat seq)
            (reduce join-frequencies {}))))))

(defn part-2-recur [input]
  (let [{:keys [template insertions]} input]
    (->> (count-pairs-top-down insertions 40 template)
         (map val)
         ((juxt (partial reduce max) (partial reduce min)))
         (reduce -))))

(defn next-pairs [input]
  (letfn [(update-pair [acc [pair number]]
            (if-let [middle-ch (get (:insertions input) pair)]
              (->> (interpose middle-ch pair)
                   (partition 2 1)
                   (reduce (fn [acc pair] (update acc pair (fnil + 0) number)) acc))
              (update acc pair (fnil + 0) number)))]
    (update input :template #(reduce update-pair {} %))))

(defn update-or-remove [m k f x]
  (let [m (update m k f x)]
    (if (zero? (get m k))
      (dissoc m k)
      m)))

(defn match? [pair [other-pair _]]
  (or (= (second pair) (first other-pair))
      (= (first pair) (second other-pair))))

(defn join-pairs [pair other-pair]
  (if (= (second pair) (first other-pair))
    [(first pair) (second other-pair)]
    (when (= (first pair) (second other-pair))
      [(first other-pair) (second pair)])))

(defn common-character [pair other-pair]
  (if (= (second pair) (first other-pair))
    (second pair)
    (when (= (first pair) (second other-pair))
      (first pair))))

(defn count-characters [acc template]
  (if (empty? template)
    acc
    (let [[pair num-1] (reduce (fn [a b] (max-key val a b)) template)
          [other-pair num-2] (->> (update-or-remove template pair - 1)
                                  (filter (partial match? pair))
                                  (sort-by (comp - val))
                                  (first))]
      (if-not other-pair
        (recur (reduce (fn [acc ch] (update acc ch (fnil + 0) num-1)) acc (set pair))
               (update-or-remove template pair - num-1))
        (recur (update acc (common-character pair other-pair) (fnil + 0) (min num-1 num-2))
               (-> template
                   (update-or-remove pair - (min num-1 num-2))
                   (update-or-remove other-pair - (min num-1 num-2))
                   (update (join-pairs pair other-pair) (fnil + 0) (min num-1 num-2))))))))

(defn part-2-by-counting-pairs [input]
  (->> (update input :template #(frequencies (partition 2 1 %)))
       (iterate next-pairs)
       (drop 40)
       (first)
       :template
       (count-characters {})
       (map val)
       ((juxt (partial reduce max) (partial reduce min)))
       (reduce -)))

(comment
  (-> example parse part-1)
  (-> input parse part-1)

  (-> example parse part-2-bottom-up)
  (-> input parse part-2-bottom-up)

  (-> example parse part-2-recur)
  (-> input parse part-2-recur)

  (-> example parse part-2-by-counting-pairs)
  (-> input parse part-2-by-counting-pairs))
