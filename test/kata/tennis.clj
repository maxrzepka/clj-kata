(ns kata.test.tennis
 (use kata.tennis
      clojure.test))

(deftest Check-end-rule
  (let [game-end? (build-end-rule 4 2)]
    (is (= (game-end? [1 1] ) false))
    (is (= (game-end? [3 0] ) false))
    (is (= (game-end? [4 0] ) true))
    (is (= (game-end? [4 2]) true))
    (is (= (game-end? [8 6]) true))
    (is (= (game-end? [8 7]) false))))

(deftest test-rule
  (let [game-end? (:game tennis-rules)
        match-end? (:match tennis-rules)
        set-end? (:set tennis-rules)]
    (is (= (match-end? blank-score) false))
    (is (= (set-end? blank-score) false))
    (is (= (game-end? blank-score) false))))

(deftest test-add-point
  (is (= (add-point {:match [0 0] :set [[0 0]] :game [1 0]} 1) 
          {:match [0 0] :set [[0 0]] :game [1 1]}))
  (is (= (add-point {:match [0 0] :set [[0 0]] :game [1 0]} 0) 
         {:match [0 0] :set [[0 0]] :game [2 0]}))
  (is (= (add-point {:match [0 0] :set [[0 0]] :game [3 2]} 0) 
         {:match [0 0] :set [[1 0]] :game [0 0]}))
  (is (= (add-point {:game  [1 3] :match [0 0] :set [[0 0]]} 1)
        {:game  [0 0] :match [0 0] :set [[0 1]]})))

(deftest test-simple-score
  (is (= (score [0]) {:match [0 0] :set [[0 0]] :game [1 0]}))
  (is (= (score [0 1]) {:match [0 0] :set [[0 0]] :game [1 1]}))
  (is (= (score [0 1 1 1 0 0]) {:match [0 0] :set [[0 0]] :game [3 3]})))

(deftest test-tie-break
  (is (= {:match [0 0] :set [[6 6]] :game [5 0]}
       (add-point {:match [0 0] :set [[6 6]] :game [4 0] } 0))))
