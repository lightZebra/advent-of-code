(ns advent-of-code.2021.p4
  (:require [clojure.string :as str]))

(def example "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n\n22 13 17 11  0\n 8  2 23  4 24\n21  9 14 16  7\n 6 10  3 18  5\n 1 12 20 15 19\n\n 3 15  0  2 22\n 9 18 13 17  5\n19  8  7 25 23\n20 11 10 24  4\n14 21 16 12  6\n\n14 21 17 24  4\n10 16 15  9 19\n18  8 23 26 20\n22 11 13  6  5\n 2  0 12  3  7")
(def input "31,88,35,24,46,48,95,42,18,43,71,32,92,62,97,63,50,2,60,58,74,66,15,87,57,34,14,3,54,93,75,22,45,10,56,12,83,30,8,76,1,78,82,39,98,37,19,26,81,64,55,41,16,4,72,5,52,80,84,67,21,86,23,91,0,68,36,13,44,20,69,40,90,96,27,77,38,49,94,47,9,65,28,59,79,6,29,61,53,11,17,73,99,25,89,51,7,33,85,70\n\n50 83  3 31 16\n47  9 94 10 86\n61 22 53 46 74\n77 41 79 55 62\n97 78 43 73 40\n\n99 96 20 35 21\n38 17 48 69 68\n 9 51 32 52 11\n67  8 42 89 27\n39 62 66 72 43\n\n33 16  4 78 31\n96 66 13 55 18\n47 89 83 99 85\n50 43 39 34 98\n81 65  7 23 17\n\n24 13 57 84 50\n83 86 98 92  7\n28 31 85 21 12\n37 48 43 47 67\n19 27  1 20 16\n\n38 75  3 14  4\n 8 86 98 94 83\n60 46 63 85 20\n69 26 73 40 29\n48 84 33 18 74\n\n13 33 37 45 22\n19 28 61 58 69\n42 14 23 39 88\n92 81 54 99 52\n57  3 34 29 62\n\n19 71 46 13 81\n99 34  8  7 89\n72 56 38 22 27\n52  2 44 12  4\n53 86 45 95 39\n\n67 12 16 60 47\n79 21 99 15 59\n81 13 64 83  4\n85 48 17 29 66\n41 97 80 51 68\n\n72 19 67  6  9\n63 80 78 97 43\n53 73 91 44 47\n 3 54 41 61 70\n69 36 57 55 45\n\n97  7 39 48 10\n77 42 65 89 79\n24 58 23 37 15\n26 71 41 18 87\n50 88 98 43  1\n\n76 50 48 10 77\n27 13 18 35 24\n31 72 41 64  2\n16 43 36 81 26\n66 51 30 34 74\n\n93 99 19 72 58\n 7 76 80 94 23\n87 59 30 77 49\n53 88 51  4 36\n90 38 64 70 46\n\n36 38 45 13 68\n12 35 57 64 29\n71 74 15  0 49\n77 21 27 84 65\n22 23 60 17 10\n\n84 31 99 93 98\n71 73 48 38 83\n12 74 34 57 45\n 2  9 76 79 77\n 0 51 72 33 29\n\n65 43 15 53 89\n75 55 99 59 48\n 6 85 68 30 39\n 5  0 47 81 95\n96 31 23 87 73\n\n23 88 98 43 56\n 4 89 53 34 41\n33 37 24 27 19\n22 83 72 75 31\n68 95 77  1 49\n\n14 48 20 73 11\n27  1  5 61 60\n96 99  9 64 29\n42 92 59 95 81\n69 97 78 86 16\n\n69 10 41 13 90\n75 95 99 72 29\n 7 85 42 77 16\n88 19  2 45 64\n14  0 83 43 70\n\n74 85 45 50  6\n92 19 43 97 65\n56 11 77  5 28\n16 10 54 44 63\n 3 93 75 12 51\n\n40 43 92 21 90\n 5 62 74 34 25\n88 47 65 37 83\n15  6 10 99 89\n78 56 35 75  9\n\n96 74 41 81 31\n35 94 48 44 21\n99 11 32 15 43\n91 34 85 23  7\n54 77 89 13 26\n\n18 58 19 28 69\n74 41 91 88 15\n11 86 44 99 45\n79 93 80 55  4\n70 56 37 84 78\n\n48 95 57 70 84\n31 73 35 77 68\n 4 53 32 63 13\n46  3 71 88 37\n72 65 36 50 49\n\n75  9 36 94  3\n71 62 65  0 15\n18 31 57 35 38\n 6 16 22 34 95\n66 29 52 73 68\n\n83 54 24 26 96\n36  2  3 34 95\n16 77 11 56 91\n80 10 93 42 59\n88 47 76 55 79\n\n51 76  4 75  7\n17 98 78 12 66\n 1 31 52 30 45\n74 29  6 87 90\n32  9 88 13 34\n\n97 92 40 73 76\n21 15 34 35 45\n 1 27 48 78 46\n95 43 17 16 20\n62 28 52 56 68\n\n16 86 55 23 30\n20 73 83 89 35\n42 38 87 59 69\n 3 79 85 43 78\n84 19 18 17 33\n\n10 43  3 68 56\n16 52 45 77 25\n75 73 66 46 82\n41 80 99 11 93\n71 79 37  5 84\n\n 9 76 96 14 52\n67 74 86 32  4\n 6 28 31 27 23\n56 58 25 69 38\n82 91 26 15 57\n\n96 62 34 67 53\n99  5 27 45 63\n80 38  0 71 43\n75 49 33 36  2\n15 21 54 20 81\n\n96 59 72  6 38\n60 70 76 82 46\n47 53 51 64 98\n44 25 69 81 33\n73 52 10 74 55\n\n52 25 99 11 60\n56 63 39 43  2\n34 45 59  8 30\n51 92 90 86 98\n19 80 47 69 13\n\n11 98 55  6 39\n70 26 99 57 75\n52 41 81  3  5\n96 92 94 35 46\n24 78 40 58 95\n\n81 87 93 88 29\n61  2 11 72 31\n60 76 19 36 58\n71 43 69 94 45\n99  9 62 48 30\n\n84 87 15 67 54\n13 81 97  8 92\n43 60  5 19  0\n91 20 69  1 29\n23  7 74 28 53\n\n73 68 24 64 47\n81 35 23 95 39\n51 69 94 37 21\n97 48 66 91 55\n56 18 49  9 86\n\n67 96 91 73 44\n77 10 50 81 19\n63 55 46 95 97\n 8 69 40 70 61\n31 20 92 98 72\n\n36 81 69 98 59\n39 15 96  9 23\n14 84 88 89 90\n45 34 22 64 50\n86 32 53 77 55\n\n20 62 23 29 77\n13  0 14 92 42\n 5 88  8  1 16\n80 79 84 49 40\n46 96 71 76 25\n\n17 65 37  3 35\n23 22 95 91 36\n61 11 51 64 85\n81 75 53 88 62\n59 14 29 73 57\n\n91 87 11 35 98\n34  1 28 27 10\n92 40 64 24 43\n55 49 42  0 36\n93 19 45 21 71\n\n82 86 14 10 43\n44 87 62 85 38\n31 67  3 68 64\n56 36 79 78 58\n21 95 35 90 18\n\n90 60 20 27 80\n39 30 12 83 96\n49  4 11 98 76\n74 37 54 26 19\n35 43 92 62 34\n\n36 23 45 24 63\n66 34 32 67 30\n26  0  5 69 50\n21 80 96 38 93\n49 46 61 41 16\n\n52 97 64 34 74\n28 46 31 56 75\n44 35 63 77  8\n 7 68 71 18 38\n61 91 49 26 15\n\n83 80 10 38 45\n81 99 30  3 63\n57 96 82 55 76\n75 41 86 94 46\n59 42 40 68 48\n\n48 43 92 50 21\n37 56  8 38 94\n73 74 35  3 52\n 7 29 82 98 86\n57 79 22  1 14\n\n53 46  4 76 28\n30 80 13 69 86\n54 70 40 77 71\n58 24 59 37 91\n45 51 43 90 74\n\n 5 33 59 78 84\n 1 90 49 72 27\n76 12 31 86 11\n74 18 52 47 19\n17 16 34 25 82\n\n41 42 21 31 44\n70 10  8 16 55\n82 60 77 89 43\n38  4 58 90 94\n74 71 93 88 61\n\n60 95 12 74 56\n82  3 48 22 27\n67 49  4 42 39\n18 35 43 87 45\n76 63 54 21 19\n\n35 89 76 86 32\n49  9  0 91 99\n87 26 97 22 44\n21 19 48 84 33\n98 30 50 90 53\n\n62 77  8 16 96\n73 65 39 79 78\n12 55 86 99 60\n 9 22 71 98  2\n24 70 75 50 41\n\n46 55 77 38 26\n70 19 72 88 23\n91 84 56 51 99\n49 69 90 48 14\n93 76 63 92 71\n\n16 76 31 17 24\n14 95 34 12 75\n37 50 74 73 41\n68 56 58 23 84\n63 26 55 15 54\n\n35 65 20 19 61\n56  3 40 66 26\n36 44 13 18 78\n 8 12  9 48 51\n 0 93 53 71 95\n\n 6 63  5 47 48\n81 86 43 73 69\n55 83 36  4 33\n23 96 88 38 32\n52 85 60 53  2\n\n27 88 14 49 89\n17 75 34 87 96\n76 48 95 60 98\n46 22 29 30  6\n 3 94 63 77 83\n\n63 98 18 73 80\n37 56 95 60 53\n 6 97 59 17 55\n20 74 24 96 79\n19 31 61  0 38\n\n93 52 54 25 51\n97 94 76 31 82\n53 74 87 65 89\n22 62 92 15 73\n17 95  1 32 43\n\n 5 44 76 22 33\n16 91 48 42 29\n10 13 25 69 51\n97  7 64 60 88\n32 86 74 39 68\n\n15 60 30 58 32\n 2 92 49 70  1\n29 90 85 93 59\n88 95 61 55 57\n19  8 97 10 45\n\n49 83 66 38 97\n68 81 69 92 47\n70 32 98  4 63\n37 25 84 80 54\n31 56 51 74 57\n\n86 75 61 68 26\n82 81 25 69 44\n62 70 23 37 43\n29 98 39 54 33\n87 93 15 79 58\n\n50 54 78 51 91\n71 70 27 28 76\n49  1 48 11 83\n98  4 56 86 67\n44 23 16 17 94\n\n84 78  3 44 96\n59 86 70 80 48\n93 88 52 43 61\n95 66 46 62 58\n 5 25  6 85 99\n\n66 40 33 10 52\n38 30 99 79 60\n75 72 59  2 53\n20 83 43 76 44\n48 46 63 15 84\n\n54 80 53 36 95\n59 41  5 82 52\n55 56 22 33 15\n37 10 81 79 27\n42 98 83 23 28\n\n94 26 80 60 62\n91 57 58 59 39\n38 29 41 86 88\n11 46 66 73 95\n78 63 12 40 89\n\n57 77 46 88 69\n45 89 71 43 35\n56 52 30 29  8\n68 39 64 66 28\n10 47 80  7 19\n\n57 37 63 90 88\n47 10 22 58 46\n95 71 24 60 23\n 0 45 75 50 77\n73 26 36  7 79\n\n26 79 66 87 72\n94 29 17 57 81\n64 91 28 27 89\n95 25  4 31 86\n85 34  6 21 76\n\n70 35 89 57 42\n34 54 64 71 61\n11 97 92 22 10\n 0 81 78  7 53\n63 65 39  2 25\n\n11  5 24 28 10\n63 35 69 49 65\n42  4 60 57  6\n 1  2 22 81 66\n70  9 86 50 64\n\n 9 73 85  6 43\n74 24 30 76 89\n38 67 60 42 78\n34 22 20 69 92\n71 79 35 17  0\n\n66 61 87 49  7\n60 25 39 69 27\n41 76 59 95 45\n16 99 64 34  1\n74 62  9 75 18\n\n24 15 47 80  0\n99 92 29 67 64\n94 27 85 97 19\n55 75 46 91 52\n32  8 76 61 14\n\n95 10 21 53 63\n94 90 56 13 71\n76 42 17 35 65\n31 29 57  8 64\n77 30 16 79 61\n\n85 57  3 67 31\n62 46 55 63 18\n95 37 71  0 24\n23 32 12 96 89\n29 17 79 82  6\n\n21 53 44 78 99\n73 98 85 41  8\n39 19 28 27 81\n75 38 37 74 66\n47 46  6 29 14\n\n58 13 76 91 23\n 1 99 81 69 86\n45 36 22 53 16\n30 71 89 18 49\n87 95 60 75 98\n\n30 61 64 54 80\n22 47 84 16  8\n83 18 65 70 11\n81 23 98 26 82\n45 69  6 53 68\n\n38 29 43 78 85\n67 39 99 98 52\n76 82 51  3 72\n46 19 65 93 34\n90  0  7 20 74\n\n85  6 67 50 45\n75 79 32  2 94\n22 60 95 34 78\n90  3 58 98 61\n63 26 76 42 89\n\n28 64 47 36  5\n76 41 26 79 10\n14 56 92 95 22\n32 54 13 98 19\n45 11 69 71 20\n\n90 46 64 38 73\n48 49 28 45 98\n77 30 35 81 78\n32 92 19 34 12\n69 74  6 89 61\n\n36 10 29 33 37\n64  7 81 31 79\n56 15 28 51 78\n 2 92 50  9 23\n48 73 32  4 39\n\n86 82 78 41 21\n22 66 65  0 47\n46 43 29 77 45\n37 88 49 90 19\n40 10 96 13 38\n\n96 30 45 80 77\n27 82 83 64 22\n24 56 11 20 51\n55 54  2 59 14\n76 67 90 93 46\n\n11 50 90 29 33\n92 81  8 19 47\n25 66 74 22 73\n28  3 97 40 67\n53 71 48 49 57\n\n26 78 35 27 66\n98 10 88 43 86\n93 30 75 46 56\n23 92 34  4 85\n28 38 42  3 39\n\n28 96 83 99 97\n61 41 73 48 23\n44  7 89 49 60\n39 76 85 26  9\n82 53 98  2 15\n\n84 57 27 91 69\n20 43 13  9 61\n28 18 17 71  6\n48 58 55 96 24\n56 95 34 33 15\n\n24 49 88 55 75\n39 95 59 80 51\n35  0 56  7 25\n 9  1 77 64 18\n50 34 54 57 99\n\n60 78 56 14 90\n44 30 48 15 12\n22 54  2 33 79\n34  4 76 93 29\n38 58 35 18  5\n\n81 22  3 41 80\n 0 77 72 87 30\n97 99 38 69 13\n91 71 24 56  9\n36 44 21 79 53\n\n88 31 62 15 77\n25 39 37 53 20\n44  0 48  4 47\n29 73 49  8 72\n68 79 84 56 41\n\n86 48 70 56 67\n68  7 73 55 10\n38 82 65 22 62\n51  2 34 17 53\n47  0 28 39 83\n\n27 18 39  0 48\n84 74 64 80 60\n28 96 37 65 57\n53 79 89 32 14\n55 63 50  7 62")

(defn parse [input]
  (let [[numbers & grids] (filter seq (map str/trim (str/split-lines input)))]
    {:numbers (mapv #(Integer/parseInt %) (str/split numbers #","))
     :grids   (->> grids
                   (map #(str/split % #"\s+"))
                   (map (partial map #(Integer/parseInt %)))
                   (partition 5))}))

(defn grid-sequences [grid]
  (->> (apply map vector grid)
       (map set)
       (into (mapv set grid))))

(defn winning-grid-position? [numbers grid]
  (loop [position          0
         winning-sequences (grid-sequences grid)]
    (if (some empty? winning-sequences)
      (dec position)
      (when (< position (count numbers))
        (recur (inc position)
               (map #(disj % (get numbers position)) winning-sequences))))))

(defn winning-grid-number [state]
  (let [{:keys [numbers grids]} state]
    (->> (map #(winning-grid-position? numbers %) grids)
         (keep-indexed vector)
         (sort-by second)
         (ffirst))))

(defn part-1 [input]
  (let [{:keys [numbers grids]} input
        grid-number      (winning-grid-number input)
        winning-grid     (nth grids grid-number)
        winning-position (winning-grid-position? numbers winning-grid)
        enough-numbers   (take (inc winning-position) numbers)]
    (* (get numbers winning-position)
       (->> (set (apply concat winning-grid))
            (remove (set enough-numbers))
            (reduce + 0)))))

(defn losing-grid-number [state]
  (let [{:keys [numbers grids]} state]
    (->> (map #(winning-grid-position? numbers %) grids)
         (keep-indexed vector)
         (sort-by (comp - second))
         (ffirst))))

(defn part-2 [input]
  (let [{:keys [numbers grids]} input
        grid-number      (losing-grid-number input)
        winning-grid     (nth grids grid-number)
        winning-position (winning-grid-position? numbers winning-grid)
        enough-numbers   (take (inc winning-position) numbers)]
    (* (get numbers winning-position)
       (->> (set (apply concat winning-grid))
            (remove (set enough-numbers))
            (reduce + 0)))))

(comment

  (part-1 (parse example))
  (part-1 (parse input))
  (part-2 (parse example))
  (part-2 (parse input)))