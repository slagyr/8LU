(ns qsort)

(defn aswap! [ary a-index b-index]
  (let [a (aget ary a-index)
        b (aget ary b-index)]
    (aset ary a-index b)
    (aset ary b-index a)
    ary))

(defn- pivot-around! [ary pivot-value range new-pivot]
  (if (empty? range)
    new-pivot
    (let [i (first range)]
      (if (< 0 (compare pivot-value (aget ary i)))
        (do
          (aswap! ary i new-pivot)
          (recur ary pivot-value (rest range) (inc new-pivot)))
        (recur ary pivot-value (rest range) new-pivot)))))

(defn- pivot! [ary left right pivot]
  (let [pivot-value (aget ary pivot)]
    (aswap! ary pivot right)
    (let [new-pivot (pivot-around! ary pivot-value (range left right) left)]
      (aswap! ary new-pivot right)
      new-pivot)))

(defn qsort! [ary left right]
  (when (> right left)
    (let [pivot (pivot! ary left right left)]
      (qsort! ary left (dec pivot))
      (qsort! ary (inc pivot) right))))

(defn qsort [values]
  (let [ary (into-array Object values)]
    (qsort! ary 0 (dec (alength ary)))
    (or (seq ary) [])))
