(ns kata.tennis)

(defn build-end-rule [min-point-to-win point-gap & [limit]]
  {:pre [(> min-point-to-win point-gap)]}
  (fn[[s1 s2]]
      (and (>= (max s1 s2)  min-point-to-win)
           (>= (max (- s2 s1) (- s1 s2)) point-gap))))

(def tennis-rules
  {:match (comp (build-end-rule 2 1) :match)
   :set (comp (build-end-rule 6 2) last :set)
   :game (comp (build-end-rule 4 2) :game)
   :tie-break (comp (build-end-rule 7 2) :game)})

(def blank-score {:match [0 0] :set [[0 0]] :game [0 0]})

;; copy of http://clj-me.cgrand.net/2011/06/17/a-flatter-cond/
(defmacro condf
  "A variation on cond which sports let bindings:
     (cond
       (odd? a) 1
       :let [a (quot a 2)]
       (odd? a) 2
       :else 3)"
  [& clauses]
  (when-let [[test expr & clauses] (seq clauses)]
    (if (= :let test)
      `(let ~expr (condf ~@clauses))
      `(if ~test ~expr (condf ~@clauses)))))


(defn add-point[score player]
  (if (:winner score)
    score
    (let [match-end? (tennis-rules :match)
          set-end?   (tennis-rules :set)
          game-end?  (tennis-rules :game)
          tie-end?  (tennis-rules :tie-break)
          tie-break? (fn [s] (= [6 6] (-> s :set last)))
          score (update-in score [:game player] inc)]
      (condf (and (tie-break? score) (not (tie-end? score))) score
             (not (game-end? score)) score
             ;; new game : re-init game's score, inc set score
             :let [last-set-index (dec (count (:set score)))
                   score (assoc score :game [0 0])
                   score (update-in score [:set last-set-index player] inc)]
             (not (set-end? score)) score
             ;; new set , inc match count
             :let [score (update-in score [:match player] inc)]
             ;; add new blank set 
             (not (match-end? score)) (update-in score [:set] conj [0 0])
             ;; end of match
             :else (assoc score :winner player)))))

(defn score[points]
  (reduce add-point blank-score points))

(defn history[points]
  (reductions add-point blank-score points))

(comment "Some REPL samples"
  (def s1 ( map #(Math/round (rand %)) (repeat 60 1)))
  (def h1 (kata.tennis/history s1))
  (kata.tennis/score ( map #(Math/round (rand %)) (repeat 60 1)))
)
