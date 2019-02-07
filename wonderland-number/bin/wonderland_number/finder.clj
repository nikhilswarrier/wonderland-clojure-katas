 (ns wonderland-number.finder)

(defn wonderland-number []
(for (vals[(range 100000 166667)])
  (println vals)
))