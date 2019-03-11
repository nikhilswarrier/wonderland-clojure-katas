 (ns wonderland-number.finder)

 (defn exp [x n]
  (reduce * (repeat n x)))
 
 (defn wonderland-number []
 (def numb 0)
 (def numb (exp 10 6))
 (def numb (/ (- numb 1) 7))
 (def wondernum numb)
 wondernum
)

