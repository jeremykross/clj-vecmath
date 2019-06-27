(ns clj-vecmath.vector
  (:refer-clojure :exclude [+ - * /])
  (:require
    [clojure.core :as c]
    [clojure.spec.alpha :as s]))

(s/def ::vector (s/coll-of number? :kind vector? :min-count 1))

(defn +
  "Add vectors"
  [& vecs]
  (apply mapv c/+ vecs))

(s/fdef +
  :args (s/+ ::vector)
  :ret ::vector)

(defn -
  "Subtract vectors"
  [& vecs]
  (apply mapv c/- vecs))

(s/fdef -
  :args (s/+ ::vector)
  :ret ::vector)

(defn *
  "Scale the vector v by the scalar s"
  [v s]
  (mapv #(c/* % s) v))

(s/fdef *
  :args (s/cat :v ::vector :s number?)
  :ret ::vector)

(defn /
  "Scale inversely the vector v by the scalar s"
  [v s]
  (mapv #(c// % s) v))

(s/fdef /
  :args (s/cat :v ::vector :s number?)
  :ret ::vector)

(defn dot
  "Compute the vector dot product"
  [v1 v2]
  (reduce c/+ (map c/* v1 v2)))

(s/fdef dot
  :args (s/and 
          (s/cat :v1 ::vector :v2 ::vector)
          #(= (count (:v1 %)) (count (:v2 %))))
  :ret ::vector)

(defn cross-3d
  "Compute the three-dimensional vector cross product"
  [[x1 y1 z1] [x2 y2 z2]]
  [(c/- (c/* y1 z2) (c/* z1 y2))
   (c/- (c/* z1 x2) (c/* x1 z2))
   (c/- (c/* x1 y2) (c/* y1 x2))])

(s/fdef cross-3d
  :args (s/and
          (s/cat :v1 ::vector :v2 ::vector)
          #(= (count (:v1 %))
              (count (:v2 %))
              3))
  :ret ::vector)

#?(:cljs
   (defn length
     "Calculate the length of the supplied vector"
     [v]
     (.sqrt js/Math (dot v v)))
   :clj
   (defn length
     "Calculate the length of the supplied vector"
     [v]
     (Math/sqrt (dot v v))))

(s/fdef length
  :args (s/cat :v ::vector)
  :ret number?)

(defn normalize
  "Normalizes vector v"
  [v]
  (/ v (length v)))

(s/fdef normalize
  :args (s/cat :v ::vector)
  :ret ::vector
  :fn #(= (length (:ret %) 1)))
