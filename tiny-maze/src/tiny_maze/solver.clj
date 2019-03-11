(ns tiny-maze.solver)

(defn get-next-positions [maze pos]
  (let [size (count maze)
        directions [ [-1 0]   
                     [1 0]     
                     [0 1]     
                     [0 -1] ]  
        inbound (fn [size [col row]]
                  (and (and (> col -1)
                            (> row -1))
                       (and (< col size)
                            (< row size))))]
    (filter #(inbound size %) (map #(mapv + pos %) directions))))

(defn get-val [maze pos]
  (let [[r c] pos]
    (get-in maze [r c])))

(defn end-node? [maze pos]
  (= :E (get-val maze pos)))

(defn valid? [maze pos]
  (let [val (get-val maze pos)]
    (or (= val :S)
        (= val :E)
        (= val 0)))
  )
(defn visited? [path pos]
  (some #(= pos %) path)
  )
(defn steps [maze path]
  (if-let [node (last path)] 
      (
        remove #(visited? path %) (filter #(valid? maze %) (get-next-positions maze node)
                                          )
              )
      ) 
  )

(defn mark-path [maze path]
  (if (empty? path)
    maze
    (recur (assoc-in maze (first path) :x) (rest path))))

(defn get-path  [maze path]
  (let [node (last path)]
    (if (end-node? maze node)
      (conj path node)
      (vec (mapcat #(get-path maze (conj path %)) (steps maze path))))))

(defn solve-maze [maze]
  (let [path (get-path maze [[0 0]])]
    (mark-path maze path)))
    
    
   
  
  
  