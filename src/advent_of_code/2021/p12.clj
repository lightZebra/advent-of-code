(ns advent-of-code.2021.p12
  (:require [clojure.string :as str]))

(def example "start-A\nstart-b\nA-c\nA-b\nb-d\nA-end\nb-end")
(def input "pn-TY\nrp-ka\naz-aw\nal-IV\npn-co\nend-rp\naw-TY\nrp-pn\nal-rp\nend-al\nIV-co\nend-TM\nco-TY\nTY-ka\naw-pn\naw-IV\npn-IV\nIV-ka\nTM-rp\naw-PD\nstart-IV\nstart-co\nstart-pn")

(defn parse [input]
  (->> (str/split-lines input)
       (map #(str/split % #"-"))
       (reduce #(conj %1 %2 (reverse %2)) [])
       (group-by first)
       (map #(update % 1 (partial mapv second)))
       (into {})))

(defn dfs [graph node path]
  (cond
    (= "end" node)
    [(conj path "end")]

    (and (= node (str/lower-case node))
         (some #{node} path))
    nil

    :else
    (->> (get graph node)
         (reduce (fn [acc edge]
                   (concat acc (dfs graph edge (conj path node))))
                 []))))

(defn small-cave-allowed? [path node]
  (let [counts (frequencies path)]
    (or (< (get counts node 0) 1)
        (and (= (get counts node) 1)
             (= (count (filter #(= % (str/lower-case %)) path))
                (count (set (filter #(= % (str/lower-case %)) path))))))))

(defn dfs-2 [graph node path]
  (when-not (and (= "start" node) (seq path))
    (when-not (and (= node (str/lower-case node))
                   (not (small-cave-allowed? path node)))
      (if (= "end" node)
        [(conj path "end")]
        (reduce (fn [acc edge] (concat acc (dfs-2 graph edge (conj path node))))
                []
                (get graph node))))))

(defn part-1 [graph]
  (count (dfs graph "start" [])))

(defn part-2 [graph]
  (count (dfs-2 graph "start" [])))

(comment
  (-> example parse part-1)
  (-> input parse part-1)
  (-> example parse part-2)
  (-> input parse part-2))
