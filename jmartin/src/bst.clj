(ns bst
  (:import
    [clojure.lang Counted IPersistentCollection ITransientCollection ISeq IPersistentSet]))

(declare bst-add)
(declare bst-remove)
(declare bst-contains?)
(declare delete-node)
(declare count-nodes)
(declare bst)
(declare left-node)
(declare empty-bst)

(deftype BST [value left right]
  Counted
  (count [this] (count-nodes this))

  ITransientCollection
  (conj [this val] (bst-add this val))
  (persistent [this] this)

  IPersistentCollection
  (empty [this] empty-bst)
  (equiv [this that] (= this that))
  (seq [this] (if (= 0 (count-nodes this)) nil this))

  ISeq
  (cons [this val] (bst-add this val))
  (first [this] (.value (left-node this)))
  (next [this] (let [result (rest this)] (if (empty? result) nil result)))
  (more [this] (delete-node this (first this)))

  IPersistentSet
	(disjoin [this key] (bst-remove this key))
	(contains [this key] (bst-contains? this key))
	(get [this key] (if (contains? this key) key nil))
  )

(defn p [node]
  (when node
    (println (.value node) (if (.left node) (.value (.left node)) nil) (if (.right node) (.value (.right node)) nil))))

(def empty-bst (BST. nil nil nil))

(defn bst-add [tree val]
  (cond
    (or (nil? tree) (nil? (.value tree))) (BST. val nil nil)
    (> 0 (compare val (.value tree))) (BST. (.value tree) (bst-add (.left tree) val) (.right tree))
    :else (BST. (.value tree) (.left tree) (bst-add (.right tree) val))))

(defn- count-nodes [tree]
  (if (and tree (.value tree))
    (+ 1 (count-nodes (.left tree)) (count-nodes (.right tree)))
    0))

(defn- left-node [tree]
  (if (or (nil? tree) (nil? (.left tree)))
    tree
    (left-node (.left tree))))

(defn- add-right-leaf [tree leaf]
  (cond
    (nil? tree) leaf
    (.right tree) (BST. (.value tree) (.left tree) (add-right-leaf (.right tree) leaf))
    :else (BST. (.value tree) (.left tree) leaf)))

(defn- add-left-leaf [tree leaf]
  (cond
    (nil? tree) leaf
    (.left tree) (BST. (.value tree) (add-left-leaf (.left tree) leaf) (.right tree))
    :else (BST. (.value tree) leaf (.right tree))))

(defn- delete-left-child [tree]
  (let [left (.left tree)
        child-left (.left left)
        child-right (.right left)
        new-left (add-right-leaf child-left child-right)]
    (BST. (.value tree) new-left (.right tree))))

(defn- delete-right-child [tree]
  (let [right (.right tree)
        child-left (.left right)
        child-right (.right right)
        new-right (add-left-leaf child-right child-left)]
    (BST. (.value tree) (.left tree) new-right)))

(defn- delete-node [tree val]
  (cond
    (or (nil? tree) (nil? (.value tree))) tree
    (> 0 (compare val (.value tree)))
    (if (and (.left tree) (= val (.value (.left tree))))
      (delete-left-child tree)
      (BST. (.value tree) (delete-node (.left tree) val) (.right tree)))
    (< 0 (compare val (.value tree)))
    (if (and (.right tree) (= val (.value (.right tree))))
      (delete-right-child tree)
      (BST. (.value tree) (.left tree) (delete-node (.right tree) val)))
    (= 0 (compare val (.value tree)))
    (if (.left tree)
      (BST. (.value (.left tree)) (.left (.left tree)) (add-left-leaf (.right tree) (.right (.left tree))))
      (or (.right tree) empty-bst))))

(defn bst-contains? [tree val]
  (cond
    (or (nil? tree) (empty? tree)) false
    (> 0 (compare val (.value tree))) (bst-contains? (.left tree) val)
    (< 0 (compare val (.value tree))) (bst-contains? (.right tree) val)
    (= 0 (compare val (.value tree))) true))

(defn bst-remove [tree val]
  (if (contains? tree val)
    (delete-node tree val)
    tree))

(defn bst [& elements]
  (into empty-bst elements))


