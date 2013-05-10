(ns kata.test.roman
  (use clojure.test
       kata.roman))

(deftest test_all
  (are [n roman] (= (roman-maker n) roman)
    22  "XXII"
    3772 "MMMDCCLXXII"
    494 "CDXCIV")
  (are [roman n] (= (roman-converter roman) n)
    "CCDIIIV" 302
    "CDXCIV" 494
    "IX" 9))

;; 
;; How to test all number between 1 and 9999 ?
;; 
