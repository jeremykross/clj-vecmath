(ns clj-vecmath.matrix
  (:refer-clojure :exclude [+ - *])
  (:require
    [clj-vecmath.vector :as v]
    [clojure.core :as c]
    [clojure.spec.alpha :as s]))

(defn matrix?
  "Returns true if x is a matrix"
  [x]
  (and
    (vector? x)
    (every? #(s/valid? :clj-vecmath.vector/vector %) x)
    (every? #(= (count %) (count (first x))) x)))

(s/def ::matrix matrix?)

(defn row-count
  [m]
  "Returns the number of rows in the matrix"
  (count m))

(s/fdef row-count
  :args (s/cat :m ::matrix)
  :ret ::matrix)

(defn column-count
  [m]
  "Returns the number of columns in the matrix"
  (count (first m)))

(s/fdef column-count
  :args (s/cat :m ::matrix)
  :ret ::matrix)

(defn row
  "Get row `n` in matrix `m`"
  [m n]
  (get m n))

(s/fdef row
  :args (s/and 
          (s/cat :m ::matrix :n number?)
          #(< (:n %) (row-count (:m %))))
  :ret ::matrix)

(defn column
  "Get column `n` in matrix `m`"
  [m n]
  (mapv #(get % n) m))

(s/fdef column
  :args (s/and 
          (s/cat :m ::matrix :n number?)
          #(< (:n %) (column-count (:m %))))
  :ret ::matrix)

(defn +
  "Add the provided matrices"
  [& mats]
  (apply mapv v/+ mats))

(s/fdef +
  :args (s/and
          (s/+ ::matrix)
          (fn [args]
            (println "args" args)
            (reduce #(and 
                       (= (row-count %1)
                          (row-count %2))
                       (= (column-count %1)
                          (column-count %2))) args)))

  :ret ::matrix)

(defn -
  "Subtract the provided matrices"
  [& mats]
  (apply mapv v/- mats))

(defn transpose
  "Compute the transpose of the matrix"
  [m]
  (mapv (fn [c] (column m c)) (range (column-count m))))

(defn- mult
  [m1 m2]
  (into []
    (mapv
      (fn [row]
        (mapv (fn [column]
                (v/dot row column))
              (transpose m2)))
      m1)))

(defn *
  "Multiply the provided matrices"
  [& mats]
  (reduce mult mats))

(defn *-vec
  "Multiply matrix `m` by vector `v`"
  [m v]
  (into
    []
    (flatten (mult m (transpose [v])))))
