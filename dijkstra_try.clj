(def start-node 'A)
(def end-node 'B)


(def all-path-nodes
  '(C D F G H E))


(def distances {#{'A 'C}     7.8
                #{'A 'D}      20.5
                #{'A 'B}     55.2
                #{'A 'F}     25.6
                #{'A 'G}         14.8
                #{'A 'H}        24.8
                #{'A 'E}     9.9
                #{'C 'D}  25.0
                #{'C 'B} 41.5
                #{'C 'F} 19.6
                #{'C 'G}     11.3
                #{'C 'H}    20.2
                #{'C 'E} 15.2
                #{'D 'B}  41.5
                #{'D 'F}  43.8
                #{'D 'G}      35.5
                #{'D 'H}     24.9
                #{'D 'E}  37.8
                #{'B 'F} 73.4
                #{'B 'G}     65.2
                #{'B 'H}    28.6
                #{'B 'E} 55.3
                #{'F 'G}     8.2
                #{'F 'H}    43.0
                #{'F 'E} 19.3
                #{'G 'H}        34.7
                #{'G 'E}     13.3
                #{'H 'E}    34.0})


(defn total-distance [nodes]
  (reduce + (map #(get distances (set %))
                 (partition 2 1 nodes))))


(defn full-path [start-node all-path-nodes end-node]
  (concat [start-node] (shuffle all-path-nodes) [end-node]))

(defn new-individual
  "Returns a new, random individual."
  []
  (let [genome (concat [start-node] (shuffle all-path-nodes) [end-node])]
    {:genome genome
     :error  (total-distance genome)}))


(defn best [individuals]
  (reduce (fn [i1 i2]
            (if (< (:error i1) (:error i2))
              i1
              i2))
          individuals))


(defn select [population]
  (best (repeatedly 2 #(rand-nth population))))


(defn mutate
  "Returns a mutated copy of the full-path genome."
  [full-path]
  (let [start-node (first full-path)
        end-node (last full-path)
        rest-path (drop 1 (drop-last full-path)) ;; Drop start and end
        [c1 c2] (take 2 (shuffle rest-path))] ;; Select two random nodes from the rest
    (concat [start-node] (replace {c1 c2, c2 c1} rest-path) [end-node])))


(defn make-child [population]
  (let [new-genome (mutate (:genome (select population)))]
    {:genome new-genome
     :error  (total-distance new-genome)}))


(defn report [generation population]
  (let [current-best (best population)]
    (println {:generation   generation
              :best-error   (:error current-best)
              :diversity    (float (/ (count (distinct population))
                                      (count population)))
              :best-genome  (:genome current-best)})))


(defn evolve-dijkstra [population-size generations]
  (loop [population (repeatedly population-size new-individual)
         generation 0]
    (report generation population)
    (if (>= generation generations)
      (best population)
      (recur (conj (repeatedly (dec population-size) #(make-child population))
                   (best population))
             (inc generation)))))


(evolve-dijkstra 100 50)
