(ns bloom
  (:import java.security.MessageDigest)
  (:import java.nio.charset.Charset)
  (:refer-clojure :exclude [hash contains?]))

(def message-digest (MessageDigest/getInstance "SHA1"))

(def charset (Charset/forName "UTF-8"))

(defn optimal-k
  "Optimal number K of hash functions for Bloom filter of bit size M
and number of elements N

  (m/n)ln2"
  [m n]
  (-> m
      double
      (/ n)
      (* (Math/log 2.0))
      (Math/round)))

(defn create-bloom
  "Create a Bloom filter with bit size M, number of (expected)
elements N and number of hash functions K. K can be calculated."
  ([m n k1]
     (let [bf (atom {})]
       (swap! bf assoc :m m)
       (swap! bf assoc :n n)
       (swap! bf assoc :k k1)
       (swap! bf assoc :f 0)
       bf))
  ([m n] (create-bloom m n (optimal-k m n))))

(defn bytes->num [bs]
  (->> bs
       (map #(bit-and 0xff %))
       (reduce #(bit-or (bit-shift-left %1 8) %2) 0)))

(defn hash [#^String x]
  (let [bs (.getBytes x charset)]
    (bytes->num (. message-digest digest bs))))

(defn indexes [s m k]
  (->> (range k)
       (map #(str s %))
       (map hash)
       (map #(mod % m))))

(defn add [bloom x]
  (let [s (pr-str x)
	{:keys [m k f]} @bloom
	f1 (reduce #(bit-set %1 %2)
		   f (indexes s m k))]
    (swap! bloom assoc :f f1))
  bloom)

(defn contains? [bloom x]
  (let [s (pr-str x)
	{:keys [m k f]} @bloom]
    (every? #(bit-test f %) (indexes s m k))))

(defn benchmark []
  ;; Inserting 1 mio. entries into a reasonably sized bloom filter
  ;; and retrieving them with a hit rate of 0.05. (Error rate of 0.1%)
  (println "Insert 100,000 entries")
  (let [n 100000
	m (* n 10)
	bt (create-bloom m n)
	bt1 (time (reduce add bt (range n)))]
    (println "Retrieving 100,000 entries")
    (let [lim (/ n 0.05)] ;; hit rate 0.05
      (time (doseq [x (range n)]
	      (contains? bt1 (Math/round (rand lim))))))))
