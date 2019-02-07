(ns alphabet-cipher.coder
 (:require
   [clojure.string :refer [index-of]]))

;;key value pair of alphabets with corresponding numbers.
(def alphaencode(into (sorted-map) (for [x (range 97 123)
                                         :let [y (char x)]
                                         :when (> x 0)]
                                     (hash-map (keyword(str y)) (- x 96)) )))


(def alphas "abcdefghijklmnopqrstuvwxyz")
  
(defn charno [c] (- (int c) (int \a)))

(defn mod26alpha [op c1 c2] (char (+ (mod (op (charno c1) (charno c2)) 26) (int \a))))

;; function to encode the message
(defn encode [key message]
(def enc "")  
  (if (=(count message) (count key))
    (do 
      (loop [i 0 j 1]
        (if (>= (count key) j)
          (let [
                mess (subs message i j)
                ke (subs key i j)
                 a (get alphaencode(keyword ke))
                 b (get alphaencode(keyword mess))
                 c (subs alphas (- b 1) 26)
                 d (subs alphas 0 (- b 1))
                 e (str c d)
                ]
            (def enc (str enc (get e ( - a 1))))
              (recur (inc i) (inc j)))
                  )      
        )enc
     )
    
    (let [re (mod (count message)(count key))
          quo (quot (count message) (count key))
           f(reduce str(repeat quo key ))
           v (reduce str (subs key 0 re))
           x(reduce str(str f v))]    
           (loop [i 0 j 1]
             (if (>= (count x) j)
               (let [
                     mess (subs message i j)
                     ke (subs x i j)  
                      a (get alphaencode(keyword ke))
                      b (get alphaencode(keyword mess))
                      c (subs alphas (- b 1) 26)
                      d (subs alphas 0 (- b 1))
                      e (str c d)
                     ]
                 (def enc (str enc (get e ( - a 1))))
                   (recur (inc i) (inc j)))
                       ))enc
          )
    )
  )

  (defn decode [key message]
     (
       (def alphadecode(into (sorted-map) (for [x (range 97 123)
                :let [y (char x)]
                :when (> x 0)]
                (hash-map (keyword (str(- x 96))) y) )))
       
    (let [re (mod (count message)(count key))
          quo (quot (count message) (count key))
           f(reduce str(repeat quo key ))
           v (reduce str (subs key 0 re))
           z(reduce str(str f v))]
      
          (def dcd "")
          (loop [i 0 j 1]
        (if (>= (count z) j)
          (let [
                mess (subs message i j)
                ke (subs z i j)  
                 a (get alphaencode(keyword ke))
                 b (get alphaencode(keyword quo))
                 c (subs alphas (- a 1) 26)
                 d (subs alphas 0 (- a 1))
                 e (str c d)
                 
                   indx (index-of e mess)
                ]
            (def dcd (str dcd (get alphadecode(keyword (str (+ 1 indx))))))
              (recur (inc i) (inc j))
          )))
) dcd
  ))

  
  
  
  
  
  
  
;  (defn decipher [message1 message2]
;    (def deciph "")
;    (loop [i 0 j 1]
;        (if (>= (count message1) j)
;          (let [
;                mess (subs message2 i j)
;                ke (subs message1 i j)
;                 a (get alphaencode(keyword ke))
;                 b (get alphaencode(keyword mess))
;                 c (subs alphas (- b 1) 26)
;                 d (subs alphas 0 (- b 1))
;                 e (str c d)
;                 indx (index-of e ke)
;                ]
;            (def deciph (str deciph (get alphadecode(keyword (str (+ 1 indx))))))
;              (recur (inc i) (inc j))
;          )))(def dcp deciph)
;             (loop [i 1 j 2]
;                 (if (<= j (count dcp))
;                   (let [  
;                         f (subs dcp 0 i)
;                         s (subs dcp i j)]
;                         (if (= f s) 
;                            (let [ 
;                                   c (count dcp) 
;                                   d j
;                                   r (- c d)]
;                                   (if (> r j)
;                                     (recur (+ i 1) (+ j 2))
;                                       (let [
;                                 q (subs dcp 0 r)
;                                 e (subs dcp j c)]
;                                 (if (= q e)
;                                   (def getdcp f)
;                                   (def getdcp dcp)
;                                  
;                                  )
;                                 )
;                          )
;                                  
;                        )
;         
;                       (recur (+ i 1) (+ j 2))
;                   )
;              )))
; getdcp )

(defn decipher [cipher message]
  (let [keystream (apply str (map #(mod26alpha - %1 %2) cipher message))]
(first (filter #(= message (decode % cipher)) (map #(subs keystream 0 %) (range 1 (count keystream)))))))
    
    
    
    
