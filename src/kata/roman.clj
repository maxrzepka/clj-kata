(ns kata.roman)

(def TABLE [{:roman \M :arabic 1000}
            {:roman \D :arabic 500}
            {:roman \C :arabic 100}
            {:roman \L :arabic 50}
            {:roman \X :arabic 10}
            {:roman \V :arabic 5}
            {:roman \I :arabic 1}
            {:roman "" :arabic 0}])

(def roman-to-arabic
  (reduce conj (for[ {r :roman a :arabic} TABLE] {r a})))

(def arabic-to-roman
  (reduce conj (for[ {r :roman a :arabic} TABLE] {a r})))

;; based on http://clojure.roboloco.net/?p=722
(defn roman-converter[roman]
  (:total
   (reduce (fn roman-add[{:keys [total base]} e]
             (let [op (if (> base e) - +)]
               {:total (op total e) :base (max base e)}))
           {:total 0 :base 0}
           (reverse (map roman-to-arabic roman)))))

(defn roman-converter-naive[roman]
  (reduce + (map roman-to-arabic roman)))

(defn decompose-digit 
  "Decomposes digit in a roman way"
  [n]
  {:pre [integer? (> 10 n -1)]}
  (cond (= n 0) [n]
        (> 4 n) (repeat n 1)
        (= 4 n) [1 5]
        (= 5 n) [5]
        (> 9 n 5) (cons 5 (repeat (- n 5) 1))
        (= 9 n) [1 10]
        :else [n]))

(defn roman-maker
  " Build a roman number as follows
    1043 --> [1 0 4 3]
         --> [[1] [0] [1 5] [1 1 1]]
         --> [ 1000 0 10 50 1 1 1]
         --> [ M  '' X C I I I]
         --> 'MXCIII'
"
  [n]
  (->> (str n)
       (map #(Integer/parseInt (str %)))
       (map decompose-digit)
       reverse
       (map (fn[b d] (map (partial * b) d))
            (iterate (partial * 10) 1))
       reverse
       flatten
       (map arabic-to-roman)
       (apply str)))

