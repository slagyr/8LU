(ns bst-spec
  (:use
    [speclj.core]
    [bst])
  (:import
    [bst BST]))

(describe "Binary Search Tree"

  (it "creates an empty tree"
    (let [tree (bst)]
      (should-not= nil tree)
      (should= bst.BST (class tree))))

  (it "can count elements in tree"
    (should= 0 (count (bst)))
    (should= 1 (count (bst 1)))
    (should= 2 (count (bst 1 2)))
    (should= 3 (count (bst 1 2 3))))

  (it "correctly builds simple tree"
    (let [tree (bst 2 3 1)]
      (should= 2 (.value tree))
      (should= 1 (.value (.left tree)))
      (should= 3 (.value (.right tree)))))

  (it "knows the first value"
    (should= 1 (first (bst 2 3 1)))
    (should= 4 (first (bst 9 8 7 6 5 4))))

  (it "knows the rest"
    (should= 2 (first (rest (bst 2 3 1))))
    (should= 3 (first (rest (rest (bst 2 3 1)))))
    (should= [2 3] (rest (bst 2 3 1)))
    (should= [5 6 7 8 9] (rest (bst 9 8 7 6 5 4))))

  (it "knows what it contains"
    (let [tree (bst 4 2 6)]
      (should= false (contains? tree 1))
      (should= true (contains? tree 2))
      (should= false (contains? tree 3))
      (should= true (contains? tree 4))
      (should= false (contains? tree 5))
      (should= true (contains? tree 6))
      (should= false (contains? tree 7))))

  (it "knows when it's empty"
    (should= true (empty? (bst)))
    (should= false (empty? (bst 1))))

  (it "removes items"
    (should= [2 3] (bst-remove (bst 2 3 1) 1))
    (should= [1 3] (bst-remove (bst 2 3 1) 2))
    (should= [1 2] (bst-remove (bst 2 3 1) 3))
    (should= [2 3] (bst-remove (bst 2 3) 1))
    (should= [3] (bst-remove (bst 2 3) 2))
    (should= [2] (bst-remove (bst 2 3) 3))
    (should= [] (bst-remove (bst 2) 2))
    (should= [] (bst-remove (bst) 2)))

  (it "is persistent"
    (let [tree (bst 4 2 6)
          conj-tree (conj tree 3)
          deleted-tree (bst-remove tree 4)]
      (should= 3 (count tree))
      (should= 4 (count conj-tree))
      (should= 2 (count deleted-tree))
      (should= nil (.right (.left tree)))
      (should= 3 (.value (.right (.left conj-tree))))
      (should= nil (.left deleted-tree))))

  (it "con hold strings"
    (let [tree (bst "one" "two" "three" "four" "five")]
      (should= ["five" "four" "one" "three" "two"] tree)
      (should= ["five" "four" "three" "two"] (bst-remove tree "one"))
      (should= true (contains? tree "two"))
      (should= false (contains? tree "foo"))))

  (it "hadles large shuffled entries"
    (should= (range 100) (apply bst (shuffle (range 100))))
;    (should= (range 1000) (apply bst (shuffle (range 1000))))
    )

  (it "deletes stuff out of the middle of a larger tree"
    (let [source (range 50)]
      (loop [tree (apply bst (shuffle source)) deletes (shuffle source) model source]
        (when (seq model)
          (let [rotten-leaf (first deletes)
                pruned-tree (bst-remove tree rotten-leaf)
                pruned-model (remove #(= rotten-leaf %) model)]
            (should= pruned-model pruned-tree)
            (recur pruned-tree (rest deletes) pruned-model))))))

  (it "balances a simple outside inbalance on the left"
    (let [tree (bst 3 2 1)]
      (should= 2 (.value tree))
      (should= 1 (.value (.left tree)))
      (should= 3 (.value (.right tree)))))

(it "balances a simple outside inbalance on the right"
    (let [tree (bst 1 2 3)]
      (should= 2 (.value tree))
      (should= 1 (.value (.left tree)))
      (should= 3 (.value (.right tree)))))


  (it "can calculate height of a tree"
    (should= 0 (bst-height (bst)))
    (should= 1 (bst-height (bst 1)))
    (should= 2 (bst-height (bst 2 1)))
    (should= 2 (bst-height (bst 2 1 3))))

  (it "can jiggle left child"
    (let [tree (jiggle-left-child (BST. 3 (BST. 2 (BST. 1 nil nil) nil) nil))]
      (should= 2 (.value tree))
      (should= 1 (.value (.left tree)))
      (should= 3 (.value (.right tree)))))
)

(run-specs)
