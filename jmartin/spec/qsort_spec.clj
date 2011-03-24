(ns qsort-spec
  (:use
    [speclj.core]
    [qsort]
    [clojure.string :only (split)]))

(def tau [6 2 8 3 1 8 5 3 0 7 1 7 9 5 8 6 4 7 6 9 2 5 2 8 6 7 6 6 5 5 9 0 0 5
          7 6 8 3 9 4 3 3 8 7 9 8 7 5 0 2 1 1 6 4 1 9 4 9 8 8 9 1 8 4 6 1 5 6
          3 2 8 1 2 5 7 2 4 1 7 9 9 7 2 5 6 0 6 9 6 5 0])

(def sorted-tau [0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 3 3 3 3 3 3 4 4
                 4 4 4 4 5 5 5 5 5 5 5 5 5 5 5 6 6 6 6 6 6 6 6 6 6 6 6 6 7 7 7
                 7 7 7 7 7 7 7 8 8 8 8 8 8 8 8 8 8 8 9 9 9 9 9 9 9 9 9 9 9])

(describe "Quick Sort"

  (it "sorts an empty list"
    (should= [] (qsort [])))

  (it "swaps"
    (should= [3 2 1] (seq (aswap! (into-array [1 2 3]) 0 2)))
    (should= [3 2 1] (seq (aswap! (into-array [1 2 3]) 2 0)))
    (should= [2 1 3] (seq (aswap! (into-array [1 2 3]) 0 1)))
    (should= [2 1 3] (seq (aswap! (into-array [1 2 3]) 1 0))))

  (it "sorts a list of 1 item"
    (should= [1] (qsort [1])))

  (it "sorts a list of 2 items"
    (should= [1 2] (qsort [2 1]))
    (should= [1 2] (qsort [1 2])))

  (it "sorts a list of 3 items"
    (should= [1 2 3] (qsort [3 2 1]))
    (should= [1 2 3] (qsort [1 2 3]))
    (should= [1 2 3] (qsort [3 1 2]))
    (should= [1 2 3] (qsort [1 3 2])))

  (it "sorts tau"
    (should= sorted-tau (qsort tau)))

  (it "sorts some strings"
    (should= ["Four" "ago" "and" "score" "seven" "years"] (qsort (split "Four score and seven years ago" #" ")))
    (should= ["To" "be" "be" "in" "is" "mind" "nobler" "not" "or" "question" "that" "the" "the" "tis" "to" "whether"]
      (qsort (split "To be or not to be that is the question whether tis nobler in the mind" #" "))))

  (it "sorts all types of numbers"
    (should= [-1 0 0.0010 1/3 3.14 9 42] (qsort [9 1/3 3.14 0.001 0 -1 42])))

  )
