(ns advent-of-code.2021.p10
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def example "[({(<(())[]>[[{[]{<()<>>\n[(()[<>])]({[<{<<[]>>(\n{([(<{}[<>[]}>{[]{[(<()>\n(((({<>}<{<{<>}{[]{[]{}\n[[<[([]))<([[{}[[()]]]\n[{[{({}]{}}([{[{{{}}([]\n{<[[]]>}<{[{[{[]{()[[[]\n[<(<(<(<{}))><([]([]()\n<{([([[(<>()){}]>(<<{{\n<{([{{}}[<[[[<>{}]]]>[]]")
(def input "<([<({{[<{{((<()>({}{})}<({}<>)>)}}>[<{(([()<>]<<>{}>))}<{({<>[]}<<>>)[([]<>)<{}{}>]}>>]]}}[<[{{[{{[<><>]\n[<{{{[((({<<<[()[]]{()()}><{<><>}(<>{})>><<{<><>}{{}[]}>{(()())[[]{}]}>>{[[(<>[]){<>{}}][(()())(()[])]]\n(((<{<[[<[{(((<><>)[{}{}]){{{}<>}[()<>]})(([<>()][<>{}]){{[]()}})}[({([][])<{}<>>}{[[]<>]{()\n[(<{<[<<<<[[{<[]()><<>()>}[{{}<>}<()>]]<<[[]{}]>{({}())}>]((((<>())<(){}>)<{[]{}}<(){}>>)[{[[]<>]<[\n[[{(<{<<[{(<({<>{}}<()[]>){<<>[]>{<>}}><[[()[]]]{{()<>}{()<>}}])[{[(<>())]<<[]{}>(<>[])>}<[{\n(<<<[[<{({<<([{}{}](<><>)){{<><>}(())}>{<{(){}}{[][]}>[{[][]}([]())]}><([{{}[]}[[]{}]](<[]{}><{}<>>))((<(){\n<<<{((<<{[([<[[]{}]([][])>{[<>()][[][]]}]<<[()[]][[]{}]>[<<><>>(<><>)]>)<({{{}<>}{{}}}{{<><\n{[(<[{{{<[{({([]{}){{}{}}}[({}<>)])[([()[]]<[]{}>)<[{}{}]{{}()}>]}(<{{{}<>}(()[])}>{{<()<>><[]<>>}\n[<<{({[{((({<<<><>>(()())>{{[][]}<{}()>}}[[((){})[{}()]]<[()<>]>]){{(<{}<>>[()<>]){{[][]}(()<>\n({[([([<(<<[{<<><>>({}())}[[(){}][[][]]]]>><<(({<><>}{[][]})<{()()}[[]<>]>)[{((){}){{}[]}}(<<>()>{{}()})]><<\n[[[{[<({{<[[[<{}{}>(()())]{(<>[])([][])}]({{(){}}})]<[<{{}[]}{()}>[{<><>}]][<((){})([][])>]\n[(<{<{(<<<<{{[[][]]<()[]>}){{<<>{}>}}>({[(()())(<>[])]}(<{()()}[<>]>{[[]]<<>()>}))>(<[{({}{})}][{[{}()]\n<(<<[([[((<<{<[]{}>{{}<>}}(({}())[[]{}])>(<{<>()}[()()]>>>))]{<{{<(<()<>>{{}()})([{}<>](<><>))>\n[{[<[([[<[((([()[]]<{}{}>))<{[{}()][(){}]}([{}<>]<()<>>)>)]>[{((([{}()]{()()>)<(<><>)(()[])>)[(([]()\n{[{[{<<([({<<[{}()]<()<>>>{[[][]]({}())}>}[[<{{}<>}{<>}>]])(<<{[()[]]}(((){})({}{}))>[({{}()})]>)](\n({{{<<<<(([([[<><>]<<>[]>]{<()<>><()[]>})<([<>{}])>])(<<{{()}<()[]>}<([]<>)>>{{[{}()][[]<>]}[{<>}{()()\n[{(([{<{{(<{<<[]()>{{}<>}>}[<({}[])<{}<>>><{<>()}>]>)}<{{{{<<>{}>{()()}}}[[{()()}(()<>)><<{}<>>>]}[[(<(){}>(\n[[{<[<(<[[<<({{}()}{{}})(({}{})[[]{}])>[([<>{}]({}<>))])]{<[<[[][]]<()<>>>]{([()()]{<><>})<{[]{}}[[]()]>}><\n([<<{({[({[({<{}[]>{{}<>}}){<[<>[]]>[(()())]}]{{{{{}[]>{()<>}}}<[{{}<>}<[]()>]([<>]<<><>>)>}})\n(([<[[{{(<[{{(()<>)}<<{}()>{<>{}}>}<[<[]()>[()()]]([<>()]{()})>)[(<[<><>]{{}<>}>{{{}()}{[]()}})<\n[([[[([[[<[{{<()()>{[][]}}(<[][]>)}[{<{}()>{<>[]}}]]>({[[{<><>}<[][]>]([()]{()[]})]}{([<<><>>[{}(\n(<({<[[[[(((({[]{}}<{}<>>)[<(){}>[()<>]])){<(<<>{}>)<<()<>><()[]>>>{[[()()]<[][]>][{{}()}<<>{\n{{{{[[<{<{(<(([]())[{}[]])<(()<>){{}{}}}>){{{<{}[]><{}<>>}{(()[])[<><>]}}<{{()()}<<>[]>}>}}><(<[(<{\n<<[(<<<([{<{(({}())[{}[]])(<(){}>(<><>))}>([[(<>[])(<>[])]<({}{})<{}>>](((<>())<{}[]>)[[<>\n<{{((([[<{[(({<>{}}({}())))[([[]()][{}{}])]]}(([<([][])<<>()>>]<{{<>[]}(()())}[({}())[()()]]>)\n(({[{[[[[(<<[[(){}][(){}]]<{()<>}<[][])>>{({[][]}{<>{}}){<[][]>}}><{(([]{}){{}<>}){[()<>][(\n{<[[<<([<{<<{[<>{}}{<><>}}(({}()))>[[{[]()}](<<>>{<><>})]><<([<>{}]{[][]})[<{}[]>{<><>}]>>}{[{(<<>()><{}<>\n({{{((({<<({<[<>()](()[])]}){((<{}()>{[]<>}){<[]{}>[[][]]})<[[[]{}](())](<{}<>>[{}])>}>[{{[(<>[])]}[<({}{\n{<(<[{({[[[{[[{}<>][(){}]]{{(){}}}}<<[[]()]{<>}><[{}[]]>>]{<<((){})[{}]>({<><>}{()[]})><<[{}()]{\n[[[(({<((<({<<{}>>}<[([]{})[[]]]>)<<{[()[]](<><>)}><<{[]{}}[[]]>{{()[]}{[]()}}>>><[((<(){}\n{{<{{{<<{<{<((<>[])<()[]>)<(<>{}){[][]}>><(({}<>){[][]}){[()<>]{<>{}}}>}>[((<<(){})({}())><([]\n[[[{({<[{[[<<(()<>)({}())>({<><>}[<><>])>]]<(<{<()<>>([]())}{[[]<>]{<>()}}>{<([][])((){})>([{}][()<>])})>}]<(\n<<{[{[[<([[<<[{}<>]>[({}())<()[]>]>[{<(){}><{}<>>}[<<>>({}{})]]]<<{<()[]><()()>}({{}()}[{}<>])>>]<<([([](\n{<<<(<{{[{{{<<{}[]>{()()}>{[<>[]]({}())}}}<[<([]())>{<()()>(()())}][<(()())<{}<>>>((<><>)[[][]]]]>\n[[[({<(([[(<({()<>}<[]<>>)>[({[]}{<><>})<<(){}>{<><>}>])]]<(<[<<()()]<{}()>>(<{}()>([][]))]<<{<><>}>>>){\n<<[[<({<({[<{(()[])}{{[]()}<[]{}>}>]<[(<[]{}><<>{}>)<[()[]]([]{})>]>})>((<([<[<><>]([]{}}><{<>()}<()[]>>]{\n({[[[{{([[((<<{}<>}[{}<>]>)(({{}<>}<()()>)[[[]<>]<()<>>]))<(<{[][]}>)<{{[]<>}<[]<>>}[(<><>){(){}}]>>][<\n{<[[[[[<[[{{{{()}[[][]]}[<(){}>[[][]]]}(<[{}{}]>)}(<<<()()>{<>}>>)]<([[[()<>]((){})]([[]()]<{}{}>)]([{{}(\n[<(({(((<[(<<[[]{}][<><>]>[{(){}}[[]]]><{{()[]}<<>()>}{{()}{(){}}}>)(<[([]())]<{{}{}}(())>>)]([([{()[]}[[\n(({[([{{((({{<<>[]>}}<([{}[]][[]()])[{{}[]}<<>{}>]>)<({([])<[]{}>}){(((){})[<>[]]){<<>[]>[{}()\n<{<{(<<({([([([]())(<>[])][{()<>}{<>{}}])](({<()()>{()<>}}{{[]}[[][]]})[[[{}{}][()[]]]])]}(({[[{<>()}[{}()]\n[[{[{{[({<{<(<{}<>>)[[{}{}>[<><>]]>[((())([]<>)){([]())<<><>>}]}>{{{<{<>[]}({}{})>{([]{}){[][]}}}[{{<>\n[[{[<([{([<<<(<><>){[]{}}>{{[][]}([]<>)}>><{<<<>()>{<><>}>[<[][]>([]())]}({[{}[]]{<>()}}({()()}(()<>)))>])}])\n{{<((([<[({{(([]){[][]})[(()<>)<()()>]}{{{()[]}}([()[]][{}{}])}})<{({[()[]]<[]{}>}[<()()><()[]>])\n([<<<([{{<({{[{}<>][<>()]}{([]<>){(){}}}}){({<{}()>}[(()){<><>}])}><{<(<()[]>({}{}))(({}{})\n{{[([<[{<<{<{[{}[]][(){}]}[[[]()]{()[]}]>}><[(([<>{}]){{<>()}{()<>}}){[{<>{}}[[]<>]]{<[]<>>}}][[[{[]<\n{<<<<{(((<{([{{}{}}][[[]()]<<>()>])<([()<>]([]))[[<><>][{}()]]>}{<{{(){}}<()<>>}<{<>{}}{<>()}>>(<{\n<[({<{{{<((<<({})([]<>)>((<>[]){[]()}}>)(<[<(){}>({}[])]>[<({}<>){[][]}>[{()<>}<<><>>]]))>[({[([<>{}][{}()])<\n(({[[{<{[[((<[<>()]><{()<>}[[]<>]>)([{[]()}[[]()]](({}{}){<>()})>){<{(()())(<>[])}[<()[]>[()()]\n[(([{{<{<[{{{[{}{}]<(){}>}[<[]{}>({}())]}({[()[]]{[]()}}[<{}[]>(<>)])}[{{<[]{}>(()<>)}<[[]()]<<>[]>)}<<<<>()\n<(<<{<([([(<[{{}<>}<()()>][{()[]}(<><>)]>{{<[]<>><{}[]>>{{{}}{[]<>}}})({[<[]{}>[[][]]]}{{<\n((<<<([[<<{([<<>{}>{<>()}])<<{[]{}}[()[]]>>}[<{([]())[<>{}]}>{((()[]){{}<>))[<()<>>([]<>)]}]>{(({[[]]\n{<<((({({[<[[[<><>][<>{}]]{({}<>)<()[]>}][<<[]{}}<[]()>>[[[]<>]]]>([[[{}{}]]<<<><>><<>{}>>\n[{<([{{<{(<{{{(){}}{{}[]}}(<<>()>[()[]])}{{{{}}(()())}{[()<>]}}>[{[{[][]}{{}<>)]}((((){}){[]{}})(({})<<>{}\n[{[(<[{<{{[<{{()()}{()[]}}<<()()><{}[]>>>][<({()()}[()[]]){([][])[<>{}]}>(((()()))((<>{})<{}{\n(<(<([<<<{[<({<>}{()()})><((<>))<<{}[]><<><>>>>]{<{[[][]]}{{[]{}}{[]<>}>>[[[[]()]({})]]}}><<<<\n<([<(<<<(({{<[<><>]<[]<>>>(<[]()>[{}<>])}[<(()()){{}<>}>]}))<<[{{[<>[]]<()[]>}{(()<>){()()\n<[(<{{[[[<(([[<>()]([][])])){{{[()()]{{}[]}}[([]{}){(){}}]}{[<[]>{[]()}][[<>]({}())]}}>]]<(<[{<{<>()}{()[\n<{{{[(<{<[<{{<()>(<>{})}[({}()){()()}]]([{<>{}}[()()]](<[]<>><<>{}>))>[<{<[]()>(<>)}<{[]{}}[(\n<{(<[<[<{<({<[<><>}[[]()]>}([<(){}><<><>>][<()()><<><>>]))([[([]())<{}>]<[()()]{<>{}}>]{(({}\n<(<({(<[<[(((<<>{}>{{}()})){{[[]()](())}{{<><>}<<><>>}})([<<()()><[]())>[[<>()]<<>[]>]][<{()()}\n((<[{{(<[([<([()()](<>{}))(<{}()>[{}{}])>({<{}{}>(<>[])})])]>)}<({((([(<[]{}>({}[]))]{((<><>)(()()))}))(({{<{\n{({[<<[(<<((<<{}[]>[()<>]>{<<>[]>{[]()}}){[<<><>>[()[]]]<<{}()>[()<>]>})[{[{{}<>}{()}]}<(<()[]><{}{}>)[[\n{{[<((<((<{[([[][]]<{}<>>)((()[]))]}>{[{(({}())[()[]]){{[][]}([][])}}](<<[<><>]{<>}><<{}<>><[][]>>>\n[[[<(<{{([{(<(<>{})(()<>)><(<>[])[{}()]>)([[[]()][[]<>]])}[[{<[]()>(<><>)}[(()){<>()}]]({{[]()}})]])[[\n<({([{(<{{[<<[(){}][[]()>>[[{}{}](()[])]>]{{[[{}<>]{{}<>}]{({}<>)[{}{}]}}[({()()}[{}<>]){((){})}]}}{{[[{{\n[[[[{<[<(([<[<[]{}><<>{}>]<<[]<>>(()[])>>([(<>)<{}()>])])<{{({[]{}}{[]()})}<[[<><>]{[]<>}]{<<>()><<>()>}>\n<<<[([[(({(<[<{}>[{}()]]{{()}(()[]}}>(<<<>()>(<>[])>))<<[{<>()}{<>[]}][{[]{}}<{}[]>]>>}){<<<(<<>\n{{<[{{({{(([[([]{})<{}()>]]))){(<{{<<>[]>(<>[])}<<()><[]<>>>}>([([<>()]<{}[]>)(({}{}))]<<([]())[<>\n([<[({[{{[[[({<>{}}<(){}>)({<>[]}{()<>})](([(){}]{[]()}))]<[<(<>[])({}<>)>][[[[]{}]<()>]<<<>[]>{[]<>}>]>]\n{[{[[{<(<<({{([]<>)([]{})}<<{}<>><<>[]>>}(({()()}[{}()]){(<><>)[<>[]]]))<<[([])(<>[])]{([]())(\n({<<{{[<<[{{<[(){}][[]]><{[]()}<[]{}>>}[<<<>()>{{}()}>[<{}<>>[<><>]]]}](<<[(<>{})[()]}{<<>><[]\n<<<([{[([{{[{[[]<>][<>[]]}]}{<([[][]]){[<>[]]<{}()>}><(<[]<>>(<>{}))[<[][]>{<>}]>}}{<{{[<>{}]{()\n<((<([([<{<[([{}<>](<>()))<([]<>)([]<>)>][<{()<>}{[]<>}>]>}<{[(<[]()>(<><>))[<(){}>[[][]]]](<([]())<<>[]>>[\n{[<[<<{{(<{[{((){})<{}[]>}[([][])]]}{[({()<>}){[{}<>](<>{}))]}>)}<[[(<(<()<>>){(()())([]{})}>({[{}[]]\n<([[<({{[<({[({}{}){{}()}]<((){})[<><>]>})([[{[][]}<()<>>]<[()<>][{}{}]>])>]}}){{[(({(({[]()}{()()}\n<({([<[{(<[{(({}[]))}[[{[]<>}{{}[]}](({}[]))]]<[[(()[])[()()]]{[{}()>}]>>{<<{[[]<>](()())}[<{}[]>(<>\n(([[<(<<[{<(<[{}{}]{()<>}>)((<(){}>([]))({<>{}}[<>[]]))>(<<{()[]}{(){}}>[[<>()]]>[<{()[]}<()[]>>{({}<>){[]<\n{<[<<{(<{{<[{[{}()][<><>]}][{({}{})(<>[]))]>{<[[<><>]]<<<>()>({}[])>>({<[]<>>([]<>)}{[(){}]([]{})})\n{{<((([<{(([{[<>{}]{{}<>}}])<<<{<>[]}{(){}}>{(()[])[()[]]}>{{<<>()>}([(){}]{{}<>})}>)}{(([{{{}[]}(<>())\n{[(<{{{<[[{{[[()[]](<>{})]((()<>)({}[]))}}]][{([[{{}[]}<[]<>>]][{(<>{})}[(<>()){<><>}]])}>>[([\n<<<{([((<<{(({{}{}}({}{})))}{(<(<><>)[[]<>]>{[{}[]](<>)})<{{<>()}{{}<>}}<<<>[]><<>()>>>}>>))]){\n<{(<[[(({<[{{<<><>><{}<>>}<<<>{}>{()()}>}]>}))[((([([{[]{}}]([{}()][<><>]))(((<>())(<>]))][\n([<<[({(({{((<[]{}>{()[]}})({[<><>][<>()]}(<<><>>[[]()]))}{{{(()<>)({}{})}(<{}()><[]()>)}{{{<><>}\n[<<<{{<<{<{<<[()()](<><>)>>([({}){{}[]}]{<[]()>{{}()}})}<[{([]())<{}<>>}([{}()]<{}[]>)]<(<[]\n{{<[({({<{({((()<>)){[[]()]<(){}>>}<[[{}]{<><>}]<(()<>)<()()>>>)}(({((<><>)[{}[]]){[[]()][()[]]}})<({\n<[{<({<({(({{<(){}>[[]{}]}<<[][]><{}{}>>})[(([{}()][<>()]))[(<<>{}>([][]}){[[]{}]}]])[<[{(()<>)[{}<>]}]>]\n([(([{<(<{[<{([][]){[][]}}(<<>><[]{}>)>({[<>()]{{}<>}}(<<>[]>{<>{}}))][({{[]()}{[]}}(([]{})(()<>))\n<{<<<({[[(<<{(()<>)[[]{}]}<<()()>>>><[({()[]}[[][]]){<[][]>(<><>)}][(({}{})<()[]>){{{}()}<[]{}>}]>)[<((([]{}\n<([[([<<<{<<<<<><>>({}())>{{[]()}[{}[]]>>[<[<>()]({}{})>]>(({<{}{}>{<>{}}}{<{}[]>{{}()}}){[<[]()>{()<>}\n[{{<{[{{{<{{<([][])[()<>]>{{[]{}}({}())}}((<<>[]>([][])))}><{<<<{}<>><<>()>>{(()<>){[]{}}}>}{([{[][]}{<>{}}])\n[({<((({(<{[{<[][]}(<>[])}<<{}<>>{{}<>}>]({{<>()}[{}{}]})}(<{{{}()}<<>[]>}[([]{})<[][]>]>[{(<>\n<([[[<{{<<<[(((){})(()()))([<><>]{()})]<<[<>]([])>(([]<>))>>{{[<()>]<(()[])([][])>}[({{}}<{}{\n{<<((<[{<{<[{[[][]]{<><>}}{<(){}>{<><>}}]<((<>{})<<>[]>)>>((((<>[]){[]()})({<>()}))([[{}[]]{<>[]}]<\n(<{(<[[([[([<({}{})[()<>]>])][<{<({}())><{()()}>}>{[{[{}<>]([])}](([()()]))}]])]]>)}>[[({{\n(<[[[{<{[<([{({}{}){{}()}}[(<>{})<()()>]][{{{}()}}<{<>[]}[()<>]>]){{((<><>){()<>])<<<>()>[{}[\n<{<<[((<<<{([{{}<>}({})]{((){})[[]<>]})<[[()<>]([]())](<[]<>>({}<>))>}{(<{()[]}>{<[]<>>})<\n{[([<[<[({{[<[<>()][{}[]}>((<>())<[]<>>)]{(<{}<>>(()()))([()()])}}}<(<(<<>{}>{<>{}})><({[]{}}[\n{{[[[<<<<[[{({[]<>}<()()>)<<(){}>{<><>}>}{[([]<>)[<>[]]]}]{<[{()()}(<><>)]{{<>{}}<[]{}>}>((<{}()>{(){}}))}>>>\n({[<[[<{<([<<<()<>}<{}{}>>(<<>[]>(<>()))><{[<>()]({}<>)}<{[]{}}>>])>}{{{<{({[]{}}<<>>)<<[]<>\n([((((<(<[({((<>())<()<>>)}([<{}()>{<>()}]{{{}()}{[]<>)}))([<(<>{})><[(){}][<>()]>](([()]<[]{}>)(<{}{}>{()\n{[(<{({(<([(<[{}()][{}()]>)[[[{}()](()())]<{[][]}<[]<>>>]][({[()()]{{}{}}}((<><>)(<>())))<<{()()}{[]{}}}[{[]\n<([[{<<{<<{(([()<>]{<>[]})<<[][]>({}[])>)({<{}{}>([]<>)})}{<{{[][]}{[]{}}}[((){})((){})]><([[][]]){{()()}}>}\n(([<[[[(({{[[[[][]](<>())]{<<>><<>()>}]<<(()<>)>[<<>{}>{<><>}]>}[<(<<><>><{}{}>)[<()<>><[]()>]>[{<()[]>[<\n{[{{{[(<<[<[{[()()]<{}<>>}[([]<>)({}<>)]][<{()()}[<>[]]>]>]>>)]{([({[{({<>}(<>{}))[({}())[[][]]]}<{{()[]\n(<(<{<{<[[(({[<>[]]<{}[]>}[([]{})(()[])]))({([[]])([()()]{()[]})}<[<<><>><[]<>>]>)]]<([{{{{}<>}(<>)}}<({\n[<[{[<<[<<[<{<<>[]}<()>}{(()[])[{}[]]}><[<(){}>{{}[]}]>]>(((<{{}<>}{()[]}><<<>{}>{()[]}>)[[(\n[<{(<[<{{([{{<<>{}><[]()>}({(){}}[[]{}])}{[<[][]>[<><>]][<<><>>[{}{}]]}](<<{(){}}({}<>)>(<()[]>[()[]])>\n<[{<({((([(<([[]<>]<()[]>)<(<>())>>(<{[][]>(<>{})>[{(){}}]))])))}<<{({<<<([]{})([]())>[{()()}<()<>>]\n<[(<{((({<<{{[{}<>]}[[<>{}]({}[])]}{(<{}[]}{<>})<{[]()}[()[]]>}>>}))(<<[<<(<<>{}><<>{}>)<<")

(defn parse [input]
  (->> (str/split-lines input)
       (mapv seq)))

(def bracket-scores
  {\) 3
   \] 57
   \} 1197
   \> 25137})

(def complete-bracket-scores
  {\) 1
   \] 2
   \} 3
   \> 4})

(def brackets
  {\{ \}
   \[ \]
   \< \>
   \( \)})

(def inverse-brackets (set/map-invert brackets))

(defn find-incorrect
  ([input]
   (find-incorrect [] input))
  ([stack input]
   (when-first [c input]
     (if-let [inverse-bracket (inverse-brackets c)]
       (if-not (= (last stack) inverse-bracket)
         c
         (recur (pop stack) (rest input)))
       (recur (conj stack c) (rest input))))))

(defn part-1 [input]
  (->> input
       (keep find-incorrect)
       (map bracket-scores)
       (reduce + 0)))

(defn complete-sequence
  ([input]
   (complete-sequence [] input))
  ([stack input]
   (if-let [c (first input)]
     (if-let [inverse-bracket (inverse-brackets c)]
       (if-not (= (last stack) inverse-bracket)
         nil
         (recur (pop stack) (rest input)))
       (recur (conj stack c) (rest input)))
     (reverse (map brackets stack)))))

(defn complete-sequence-score [coll]
  (reduce
   (fn [acc bracket]
     (+ (* acc 5)
        (complete-bracket-scores bracket)))
   0
   coll))

(defn middle-element [coll]
  (nth coll (/ (count coll) 2)))

(defn part-2 [input]
  (->> input
       (keep complete-sequence)
       (map complete-sequence-score)
       (sort)
       (middle-element)))

(comment
  (->> example parse part-1)
  (->> input parse part-1)
  (->> example parse part-2)
  (->> input parse part-2))
