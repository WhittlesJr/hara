(ns hara.data.base.map)

(defn reduce-kvs [rf init kvs]
  (reduce (fn [acc [k v]] (rf acc k v)) init (partition-all 2 kvs)))

(defn dissoc-in
  "disassociates keys from a nested map. Setting `keep` to `true` will
   not remove a empty map after dissoc

   (dissoc-in {:a {:b 10 :c 20}} [:a :b])
   => {:a {:c 20}}

   (dissoc-in {:a {:b 10}} [:a :b])
   => {}

   (dissoc-in {:a {:b 10}} [:a :b] true)
   => {:a {}}"
  {:added "3.0"}
  ([m [k & ks]]
   (if-not ks
     (dissoc m k)
     (let [nm (dissoc-in (m k) ks)]
       (cond (empty? nm) (dissoc m k)
             :else (assoc m k nm)))))

  ([m [k & ks] keep]
   (if-not ks
     (dissoc m k)
     (assoc m k (dissoc-in (m k) ks keep)))))

(defn unique
  "returns a map of all key/value pairs that differ from a second map

   (unique {:a 1} {:a 2})
   => {:a 1}

   (unique {:a 1 :b 2} {:b 2})
   => {:a 1}

   (unique {:b 2} {:b 2 :a 1})
   => nil"
  {:added "3.0"}
  [m1 m2]
  (reduce (fn [i [k v]]
            (if (not= v (get m2 k))
              (assoc i k v)
              i))
          nil m1))

(defn assoc-if
  "Assoc each kv if (pred value) is true
   (assoc-if odd? {:a 1} :b 2 :c 3)
   => {:a 1 :c 3}"
  ([pred m k v      ] (if (pred v) (assoc m k v) (or m {})))
  ([pred m k v & kvs]
   (reduce-kvs
    (fn [m k v] (assoc-if pred m k v))
    (assoc-if pred m k v)
    kvs))

  ([pred m kvs]
   (apply (partial assoc-if pred m) kvs)))

(def assoc-some
  "assoc key/value pairs to the map only on non-nil values

   (assoc-some {} :a 1)
   => {:a 1}

   (assoc-some {} :a 1 :b nil)
   => {:a 1}

  (assoc-some {:a 1} :b nil :c 3)
  => {:a 1 :c 3}"
  (partial assoc-if some?))

(defn assoc-over-nil
  "assoc each kv if the value in the original map is nil or nonexistant
  
   (assoc-over-nil {:a 1} :b 2)
   => {:a 1 :b 2}

   (assoc-over-nil {:a 1} :a 2 :b 2)
   => {:a 1 :b 2}

   (assoc-over-nil {:a 1 :b nil} :a 2 :b 2)
   => {:a 1 :b 2}"
  {:added "3.0"}
  ([m k v      ] (if (some? (get m k)) (or m {}) (assoc m k v)))
  ([m k v & kvs]
   (reduce-kvs
    (fn [m k v] (assoc-over-nil m k v))
    (assoc-over-nil m k v)
    kvs))

  ([m kvs]
   (apply (partial assoc-over-nil m) kvs)))

(defn assoc-in-some
  "assoc-in a nested key/value pair to a map only on non-nil values
 
   (assoc-in-some {} [:a :b] 1)
   => {:a {:b 1}}
 
   (assoc-in-some {} [:a :b] nil)
   => {}"
  {:added "3.0"}
  [m arr v]
  (if (not (nil? v)) (assoc-in m arr v) m))

(defn assoc-in-over-nil
  "only assoc-in if the value in the original map is nil

   (assoc-in-over-nil {} [:a :b] 2)
   => {:a {:b 2}}

   (assoc-in-over-nil {:a {:b 1}} [:a :b] 2)
   => {:a {:b 1}}"
  {:added "3.0"}
  [m ks v]
  (if (not (nil? (get-in m ks))) m (assoc-in m ks v)))

(defn update-in-some
  "update-in a nested key/value pair only if the value exists
 
   (update-in-some {:a {:b 1}} [:a :b] inc)
   => {:a {:b 2}}
 
   (update-in-some {} [:a :b] inc)
   => {}"
  {:added "3.0"}
  [m arr f & args]
  (let [v (get-in m arr)]
    (if (not (nil? v))
      (assoc-in m arr (apply f v args))
      m)))

(defn merge-some
  "merges key/value pairs into a single map only if the value exists
 
   (merge-some {:a nil :b 1})
   => {:b 1}
 
   (merge-some {:a 1} {:b nil :c 2})
   => {:a 1 :c 2}
 
   (merge-some {:a 1} {:b nil} {:c 2})
   => {:a 1 :c 2}"
  {:added "3.0"}
  ([] nil)
  ([m]
   (reduce (fn [i [k v]]
             (if (not (nil? v)) (assoc i k v) i))
           {} m))
  ([m1 m2]
   (reduce (fn [i [k v]]
             (if (not (nil? v)) (assoc i k v) i))
           (merge-some m1) m2))
  ([m1 m2 & more]
   (apply merge-some (merge-some m1 m2) more)))

(defn merge-over-nil
  "only merge if the value in the original map is nil

   (merge-over-nil {:a 1} {:b 2})
   => {:a 1 :b 2}

   (merge-over-nil {:a 1} {:a 2})
   => {:a 1}"
  {:added "3.0"}
  ([] nil)
  ([m] m)
  ([m1 m2]
   (reduce (fn [i [k v]]
             (if (not (nil? (get i k)))
               i
               (assoc i k v)))
           m1 m2))
  ([m1 m2 & more]
   (apply merge-over-nil (merge-over-nil m1 m2) more)))

(defn into-some
  "like into but filters nil values for both key/value pairs
   and sequences

   (into-some [] [1 nil 2 3])
   => [1 2 3]

   (into-some {:a 1} {:b nil :c 2})
   => {:a 1 :c 2}"
  {:added "3.0"}
  [to from]
  (reduce (fn [i e]
            (if (or (and (coll? e) (not (nil? (second e))))
                    (and (not (coll? e)) (not (nil? e))))
              (conj i e)
              i))
          to from))

(defn select-keys-some
  "selects only the non-nil key/value pairs from a map
 
   (select-keys-some {:a 1 :b nil} [:a :b])
   => {:a 1}
 
   (select-keys-some {:a 1 :b nil :c 2} [:a :b :c])
   => {:a 1 :c 2}"
  {:added "3.0"}
  [m ks]
  (reduce (fn [i k]
            (let [v (get m k)]
              (if (not (nil? v))
                (assoc i k v)
                i)))
          nil ks))

(defn transform-in
  "moves values around in a map according to a table
 
   (transform-in {:a 1 :b 2}
                 {[:c :d] [:a]})
   => {:b 2, :c {:d 1}}"
  {:added "3.0"}
  [m rels]
  (reduce (fn [out [to from]]
            (let [v (get-in m from)]
              (-> out
                  (assoc-in-some to v)
                  (dissoc-in from))))
          m
          rels))

(defn retract-in
  "reversed the changes by transform-in
 
   (retract-in {:b 2, :c {:d 1}}
               {[:c :d] [:a]})
   => {:a 1 :b 2}"
  {:added "3.0"}
  [m rels]
  (reduce (fn [out [to from]]
            (let [v (get-in m to)]
              (-> out
                  (assoc-in-some from v)
                  (dissoc-in to))))
          m
          (reverse rels)))

(defn map-keys
  "changes the keys of a map
   
   (map-keys inc {0 :a 1 :b 2 :c})
   => {1 :a, 2 :b, 3 :c}"
  {:added "3.0"}
  [f m]
  (reduce (fn [out [k v]]
            (assoc out (f k) v))
          {}
          m))

(defn map-vals
  "changes the values of a map
 
   (map-vals inc {:a 1 :b 2 :c 3})
   => {:a 2, :b 3, :c 4}"
  {:added "3.0"}
  [f m]
  (reduce (fn [out [k v]]
            (assoc out k (f v)))
          {}
          m))

(defn map-entries
  "manipulates a map given the function
 
   (map-entries (fn [[k v]]
                  [(keyword (str v)) (name k)])
                {:a 1 :b 2 :c 3})
   => {:1 \"a\", :2 \"b\", :3 \"c\"}"
  {:added "3.0"}
  [f m]
  (->> (map f m)
       (into {})))

(defn transpose
  "sets the vals and keys and vice-versa
   
   (transpose {:a 1 :b 2 :c 3})
   => {1 :a, 2 :b, 3 :c}"
  {:added "3.0"}
  [m]
  (reduce (fn [out [k v]]
            (assoc out v k))
          {}
          m))
