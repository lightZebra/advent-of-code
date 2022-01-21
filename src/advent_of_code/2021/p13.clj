(ns advent-of-code.2021.p13
  (:require [clojure.string :as str]))

(def example "6,10\n0,14\n9,10\n0,3\n10,4\n4,11\n6,0\n6,12\n4,1\n0,13\n10,12\n3,4\n3,0\n8,4\n1,10\n2,14\n8,10\n9,0\n\nfold along y=7\nfold along x=5")
(def input "1094,19\n751,827\n323,890\n969,603\n301,10\n291,483\n1173,92\n1034,570\n460,732\n1222,191\n986,756\n430,480\n1057,129\n659,687\n514,803\n669,603\n234,133\n421,857\n343,725\n987,568\n232,735\n708,165\n421,737\n776,389\n1307,655\n1006,91\n259,350\n907,198\n651,80\n23,752\n629,519\n492,858\n905,110\n750,716\n184,821\n1103,171\n1307,351\n559,645\n85,485\n6,759\n459,91\n227,110\n977,835\n363,163\n437,12\n447,171\n552,417\n1094,215\n822,28\n1019,843\n621,100\n430,184\n385,571\n671,745\n1046,802\n16,175\n659,782\n907,288\n1083,784\n232,376\n269,228\n1068,82\n556,572\n870,403\n321,59\n987,288\n1007,702\n1062,705\n661,619\n1293,845\n257,77\n33,794\n684,299\n356,634\n18,592\n949,166\n1128,893\n1078,70\n490,624\n57,123\n202,133\n649,499\n22,355\n990,849\n701,842\n1077,381\n649,619\n758,480\n997,47\n363,178\n972,754\n370,201\n171,224\n328,512\n298,428\n8,515\n1041,584\n313,159\n303,779\n800,1\n1288,539\n273,588\n498,135\n796,803\n889,157\n884,546\n698,123\n1058,213\n1299,717\n648,583\n691,275\n425,466\n815,606\n144,1\n1216,600\n858,581\n1019,483\n950,5\n659,344\n888,742\n8,360\n1303,502\n380,10\n1121,731\n25,835\n291,850\n1052,126\n326,64\n669,791\n1091,709\n825,100\n627,255\n1108,514\n1062,581\n671,149\n273,59\n258,126\n291,716\n1242,574\n908,255\n70,441\n1253,571\n626,695\n703,835\n982,74\n460,221\n1108,380\n976,311\n546,236\n406,761\n1190,389\n651,687\n1190,133\n224,81\n975,616\n969,403\n1017,166\n351,219\n730,255\n412,812\n1237,801\n169,420\n1077,749\n1173,163\n303,491\n351,80\n484,759\n219,633\n652,285\n914,248\n1253,123\n16,623\n889,737\n107,204\n818,624\n728,235\n681,375\n1161,847\n233,749\n597,571\n85,37\n380,523\n276,162\n913,378\n639,337\n271,876\n1295,725\n291,411\n1222,703\n517,7\n396,752\n989,507\n333,835\n150,268\n708,298\n729,353\n303,627\n708,10\n658,138\n959,142\n780,761\n1303,54\n131,635\n179,333\n1186,380\n889,705\n1305,80\n547,409\n577,848\n94,406\n636,337\n969,495\n674,302\n455,843\n582,393\n619,275\n1275,611\n938,121\n938,773\n233,145\n313,291\n552,238\n28,199\n671,109\n746,870\n1053,77\n966,847\n612,123\n1071,390\n293,113\n1298,44\n233,189\n1031,278\n78,873\n276,732\n190,572\n1232,425\n1146,624\n490,176\n228,796\n1091,359\n487,8\n754,572\n1267,470\n679,192\n7,278\n1120,572\n1200,439\n629,234\n298,634\n315,493\n1178,513\n1019,402\n159,606\n6,79\n460,834\n897,894\n997,495\n55,87\n492,624\n880,529\n641,739\n31,707\n673,397\n800,753\n1245,297\n463,205\n1012,242\n631,432\n1275,731\n77,590\n157,786\n855,626\n826,759\n855,519\n492,270\n1200,455\n1154,31\n1268,469\n947,178\n564,870\n748,21\n199,71\n771,284\n1158,92\n50,194\n455,499\n672,623\n484,31\n218,695\n581,541\n251,59\n952,834\n1258,884\n885,428\n351,715\n1181,501\n455,338\n192,800\n497,100\n400,758\n1061,494\n1260,588\n1198,571\n986,285\n947,316\n1007,779\n426,572\n1151,606\n725,301\n144,753\n323,550\n663,54\n258,768\n324,609\n793,92\n870,732\n189,491\n586,198\n236,254\n1168,521\n708,212\n8,86\n676,32\n1292,592\n102,208\n1156,645\n137,851\n403,288\n137,802\n3,799\n683,255\n50,28\n232,824\n647,54\n669,644\n1082,796\n602,212\n1275,578\n846,572\n517,92\n199,556\n137,887\n117,362\n170,535\n323,127\n363,642\n826,863\n398,201\n1200,389\n813,49\n440,43\n800,893\n1238,235\n855,44\n909,801\n647,840\n641,644\n504,268\n607,835\n1275,316\n126,462\n383,652\n1004,696\n647,168\n28,746\n321,835\n251,835\n972,140\n987,550\n120,581\n623,259\n1160,122\n1290,696\n1026,201\n857,712\n488,28\n488,789\n922,822\n175,605\n484,135\n78,425\n512,221\n241,894\n422,152\n932,794\n584,474\n1277,794\n1009,522\n117,138\n227,784\n534,389\n1275,163\n1150,77\n741,448\n1091,466\n1178,289\n253,801\n681,234\n1238,841\n174,719\n1300,425\n661,275\n232,294\n884,348\n1275,44\n331,442\n619,163\n542,284\n189,403\n258,63\n326,830\n1176,298\n1263,840\n1260,28\n28,148\n930,10\n654,288\n256,411\n363,578\n109,56\n1297,644\n363,716\n65,752\n910,136\n1082,348\n464,572\n440,267\n967,725\n182,1\n199,395\n1004,449\n982,512\n1011,31\n962,248\n1019,716\n35,44\n497,794\n94,152\n253,577\n542,323\n281,836\n452,581\n1215,439\n319,278\n1118,800\n557,204\n564,864\n576,530\n1116,221\n239,390\n1215,455\n313,495\n1288,355\n564,248\n1263,507\n338,754\n768,571\n651,799\n1307,799\n125,182\n662,45\n644,364\n226,130\n1009,372\n197,112\n154,645\n88,703\n401,838\n505,663\n1019,411\n684,148\n806,122\n785,393\n438,837\n914,768\n1218,40\n465,611\n78,537\n363,130\n750,178\n1258,212\n1304,583\n264,473\n1176,596\n888,70\n1019,178\n1153,786\n577,366\n917,466\n1004,445\n1153,794\n254,192\n170,359\n888,152\n950,889\n1241,369\n870,715\n318,85\n855,395\n768,324\n720,572\n858,761\n248,581\n733,366\n930,371\n5,338\n341,119\n796,91\n264,802\n315,45\n954,260\n234,761\n348,646\n997,26\n688,889\n418,198\n691,619\n713,310\n1078,600\n504,570\n95,455\n125,712\n1068,530\n1113,336\n149,159\n94,600\n1014,386\n880,365\n947,700\n78,672\n405,110\n137,731\n577,304\n733,528\n962,646\n1057,765\n397,378\n959,640\n813,548\n554,354\n858,514\n1215,827\n669,103\n1009,884\n258,831\n52,212\n465,663\n233,516\n72,659\n992,361\n1175,689\n1232,537\n159,288\n50,588\n338,140\n725,605\n495,606\n788,157\n5,556\n301,884\n259,823\n363,194\n858,313\n306,445\n291,642\n1063,341\n858,133\n430,414\n952,60\n445,586\n585,605\n1019,611\n909,56\n662,583\n120,133\n651,95\n74,885\n35,731\n905,46\n1016,5\n248,189\n850,732\n915,350\n649,570\n544,10\n157,868\n1185,182\n1057,577\n388,822\n224,32\n393,466\n658,285\n321,387\n50,476\n706,570\n460,225\n1242,198\n636,302\n1081,128\n421,378\n905,784\n535,694\n582,640\n602,596\n189,439\n372,121\n266,865\n499,549\n47,840\n1140,863\n94,534\n797,399\n1307,80\n167,575\n542,324\n413,894\n1007,403\n969,851\n982,522\n331,452\n698,581\n323,568\n413,521\n318,361\n170,493\n396,696\n750,86\n602,165\n651,782\n338,469\n1304,759\n666,364\n170,759\n1153,845\n170,863\n1238,53\n884,572\n940,201\n661,324\n641,103\n1295,128\n855,338\n1220,248\n199,268\n303,563\n1121,491\n1069,894\n504,122\n741,409\n435,359\n889,378\n552,865\n542,603\n865,586\n402,639\n460,673\n1253,758\n1268,771\n947,163\n1044,29\n1078,626\n820,176\n330,285\n663,278\n709,835\n975,726\n72,841\n109,838\n1153,868\n1303,392\n995,493\n980,609\n1295,169\n912,5\n626,746\n398,693\n1153,627\n847,689\n959,219\n1007,627\n383,18\n7,616\n157,794\n1237,93\n1279,187\n982,382\n1007,675\n135,689\n1101,756\n262,572\n662,849\n455,395\n547,485\n363,252\n547,466\n641,791\n294,5\n908,191\n432,523\n107,485\n95,7\n464,322\n1146,36\n875,466\n1131,333\n440,162\n642,716\n306,198\n284,469\n969,291\n724,544\n232,268\n1128,1\n483,670\n422,600\n68,198\n1282,199\n120,761\n112,571\n258,696\n805,159\n741,37\n1307,528\n698,805\n1074,472\n1111,395\n1227,117\n663,168\n31,187\n406,581\n995,849\n639,745\n1084,764\n12,850\n1253,584\n1297,287\n328,74\n42,805\n979,792\n878,523\n734,805\n947,252\n703,333\n1297,159\n723,96\n560,178\n1279,707\n13,71\n124,380\n1166,753\n813,267\n403,507\n651,344\n1307,560\n95,887\n370,649\n530,828\n1077,378\n838,109\n162,596\n197,838\n334,364\n818,270\n510,305\n729,541\n845,611\n671,337\n783,71\n323,351\n1193,362\n1258,682\n351,752\n490,718\n261,466\n875,709\n89,166\n457,56\n69,525\n372,773\n363,499\n430,870\n589,590\n889,513\n914,70\n1092,695\n576,364\n1028,121\n1198,851\n522,605\n1143,575\n1111,402\n426,546\n1295,149\n\nfold along x=655\nfold along y=447\nfold along x=327\nfold along y=223\nfold along x=163\nfold along y=111\nfold along x=81\nfold along y=55\nfold along x=40\nfold along y=27\nfold along y=13\nfold along y=6")

(defn parse [input]
  (->> (str/split-lines input)
       (split-with seq)
       ((juxt (comp (partial mapv (partial mapv #(Integer/parseInt %)))
                    (partial map #(str/split % #","))
                    first)
              (comp (partial mapv (fn [v] (-> v
                                              (update 0 keyword)
                                              (update 1 #(Integer/parseInt %)))))
                    (partial mapv #(vec (take-last 2 %)))
                    (partial mapv #(str/split % #" |="))
                    rest
                    second)))))

(defn fold-points [input]
  (let [[points folds] input]
    (when-first [fold folds]
      (let [[direction line] fold]
        [(mapv (fn [[x y]]
                 (case direction
                   :x (if (<= x line) [x y] [(- (+ line line) x) y])
                   :y (if (<= y line) [x y] [x (- (+ line line) y)])))
               points)
         (or (next folds) [])]))))

(defn display [points]
  (let [max-x (reduce max (map first points))
        max-y (reduce max (map second points))]
    (doseq [y (range (inc max-y))]
      (doseq [x (range (inc max-x))]
        (print (if (some #{[x y]} points) "#" ".")))
      (println))))

(defn part-1 [input]
  (count (set (fold-points input))))

(defn part-2 [input]
  (->> (iterate fold-points input)
       (take-while some?)
       (last)
       (first)
       (display)))

(comment
  (-> example parse part-1)
  (-> input parse part-1)
  (-> example parse part-2)
  (-> input parse part-2))
