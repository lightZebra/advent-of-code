(ns advent-of-code.2021.p7
  (:require [clojure.string :as str]))

(def example "16,1,2,0,4,2,7,1,2,14")
(def input "1101,1,29,67,1102,0,1,65,1008,65,35,66,1005,66,28,1,67,65,20,4,0,1001,65,1,65,1106,0,8,99,35,67,101,99,105,32,110,39,101,115,116,32,112,97,115,32,117,110,101,32,105,110,116,99,111,100,101,32,112,114,111,103,114,97,109,10,273,102,514,428,300,13,9,457,92,169,1654,471,479,178,158,124,354,83,705,30,80,199,632,31,840,580,1228,1597,151,1374,1665,469,43,1113,103,1,1456,132,2,618,423,824,789,145,485,585,543,694,1266,866,1276,726,680,1714,503,79,137,208,8,1447,455,33,1098,1346,1028,178,1095,21,19,52,668,29,382,1659,310,225,21,15,258,915,434,92,181,120,408,612,684,2,5,1507,127,746,203,66,4,82,440,1796,166,515,216,422,175,1643,240,100,178,375,487,134,599,581,38,101,19,882,1580,282,228,409,1124,409,255,1004,90,123,146,1130,461,84,9,1537,240,42,812,815,72,140,299,317,398,982,340,529,521,204,1137,895,912,313,27,540,638,403,188,163,133,34,1038,1597,440,200,275,2,1057,682,108,340,1096,361,2,242,464,392,432,334,955,145,275,605,858,173,375,435,3,3,784,396,324,1004,25,794,61,1358,752,31,563,23,407,886,870,501,1353,195,751,1407,10,874,31,33,589,124,403,205,225,40,1510,150,172,74,9,715,39,14,36,1775,29,1094,1007,199,4,26,301,878,751,538,33,102,109,297,236,119,195,431,34,179,827,6,204,91,594,65,255,1136,820,163,1508,388,680,968,1587,235,294,543,197,640,143,95,28,814,1053,827,167,54,606,0,823,3,340,619,195,31,216,330,287,382,676,392,5,1233,248,1000,323,872,234,316,4,112,663,113,1402,357,416,148,53,102,681,15,294,407,269,297,386,4,254,1666,454,139,1173,65,572,132,3,368,290,271,716,646,626,1727,411,196,181,1256,92,29,4,337,207,557,425,43,465,35,85,1218,241,936,247,94,1433,1002,400,624,88,1072,1048,370,101,264,78,379,27,65,672,1368,692,822,1020,317,472,1019,298,486,2,18,163,1304,994,952,455,454,661,1,473,561,313,92,425,218,29,49,618,790,615,347,462,169,275,247,1445,1880,225,1778,159,176,569,32,529,602,34,365,84,753,253,962,137,917,401,507,242,451,751,67,20,1208,411,226,829,317,283,219,154,683,30,1092,24,386,24,117,545,35,188,621,14,1453,24,892,330,337,754,407,481,906,643,552,864,808,360,704,118,368,297,1446,1348,104,1077,588,298,141,971,2,801,74,434,663,543,872,447,368,109,292,526,933,489,65,33,1061,1030,727,718,62,31,518,457,1569,815,422,187,211,1193,256,811,88,65,275,998,618,113,208,160,113,270,1085,295,20,161,117,134,1045,132,28,29,779,1108,24,801,240,184,414,79,335,98,486,195,100,302,574,561,353,8,260,1,540,584,410,1299,266,44,1120,877,252,377,849,83,547,637,827,298,1151,222,90,533,551,203,203,67,881,6,812,88,1314,178,169,576,885,767,278,1565,154,108,543,31,100,190,298,254,1478,594,644,957,177,20,1578,482,121,106,841,195,16,51,561,205,55,97,107,380,128,655,629,995,1424,1005,276,838,143,506,450,56,172,955,20,1045,253,436,1016,1106,68,540,807,265,405,301,539,1236,874,986,1092,274,1208,738,89,107,510,90,15,1402,313,712,35,222,494,125,113,290,259,274,214,70,1416,242,1312,1023,974,128,1787,91,13,992,84,673,185,375,385,0,285,135,116,105,26,103,929,733,567,294,174,82,1181,941,161,1242,387,20,882,1789,1164,1157,936,1110,1142,1308,657,931,29,603,1001,157,22,786,161,835,459,843,50,3,51,42,476,509,1214,733,1102,1011,0,832,1186,246,284,503,455,146,398,13,1109,106,90,511,1232,1837,580,285,86,1388,1199,195,225,742,194,1448,1732,309,1074,1380,251,1010,137,382,1367,490,1828,47,11,888,251,16,37,393,1407,233,5,814,780,850,22,1196,957,492,4,254,745,580,651,225,1072,872,323,618,24,303,79,7,75,11,15,65,449,205,103,836,150,111,424,786,194,1752,55,674,1469,1050,891,50,792,0,381,31,152,141,91,1521,420,18,779,450,68,929,122,106,7,142,185,355,768,581,1024,140,438,350,1838,815,977,23,663,324,30,7,408,461,40,108,203,459,530,69,120,177,962,162,1566,253,2,308,232,42,1564,1161,48,270,253,83,652,247,539,166,159,856,331,1701,970,1085,442,21,868,960,298,534,378,75,226,586,21,57,636,279,1112,63,1519,555,191,773,168,120,603,716,544,1546,378,418,149,138,1212,575,29,284,577,223,145,387,639,1204,154,613,48,116,307,341,716,3,831,987,629,1338,852,189,340,1213")

(defn parse [input]
  (->> (str/split input #",")
       (map #(Integer/parseInt %))))

(defn part-1 [input]
  (let [min-x (reduce min input)
        max-x (reduce max input)]
    (->> (range min-x (inc max-x))
         (map #(map (partial - %) input))
         (map (partial map #(Math/abs ^int %)))
         (map (partial reduce + 0))
         (reduce min))))

(defn part-2 [input]
  (let [min-x     (reduce min input)
        max-x     (reduce max input)
        fuel-cost (fn [point position]
                    (let [delta (Math/abs ^int (- point position))]
                      (int (* (inc delta) (/ (double delta) 2.0)))))]
    (->> (range min-x (inc max-x))
         (map #(map (partial fuel-cost %) input))
         (map (partial map #(Math/abs ^int %)))
         (map (partial reduce + 0))
         (reduce min))))

(comment
  (-> example parse part-1)
  (-> input parse part-1)
  (-> example parse part-2)
  (-> input parse part-2))
