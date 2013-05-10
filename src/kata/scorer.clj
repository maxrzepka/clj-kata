(ns kata.scorer)

;; Describe any scorer with 2 players as a tree
;; {:winner <0,1> , :score {:main [] , [][[],[][]] }
;;

(defn winning-rule
  ""
  [bound gap & [limit]]
  (fn[[p1 p2]]
    (let [upper (max p1 p2)
          diff (max (- p2 p1) (- p1 p2))]
    (and (>= upper bound)
         (if limit
           (and (> diff 0) (>= upper limit))
           (>= diff  gap))))))
           
  