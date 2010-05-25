(ns bloom
  (:import java.security.MessageDigest)
  (:import java.nio.charset.Charset)
  (:import java.util.BitSet)
  (:refer-clojure :exclude [hash contains?]))

(def message-digest (MessageDigest/getInstance "SHA1"))

(def
 #^{:tag Charset}
 charset (Charset/forName "UTF-8"))

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
     (atom {:m m :n n :k k1 :f (BitSet. m)}))
  ([m n] (create-bloom m n (optimal-k m n))))

(defn bitset [& more]
  (let [bs (BitSet.)]
    (doseq [n more]
      (.set bs n))
    bs))

(defn bitset->num [bitset]
  (loop [i 0 r 0]
    (let [h (.nextSetBit #^BitSet bitset i)]
      (if (not (= -1 h))
	(recur (inc h) (bit-set r h))
	r))))

(defn abit-set [byte-array i]
  (let [len (alength byte-array)
	apos (dec (- len (int (/ i 8))))
	bpos (rem i 8)]
    (aset-byte byte-array apos
	       (-> byte-array (aget apos) (bit-set bpos) byte))
    byte-array))

(defn bytes->num [bs]
  (->> bs
       (map #(bit-and 0xff %))
       (reduce #(bit-or (bit-shift-left %1 8) %2) 0)))

(defn hash [x]
  (let [bs (.getBytes #^String x charset)]
    (bytes->num (. #^MessageDigest message-digest digest bs))))

(defn indexes [s m k]
  (->> (range k)
       (map #(str s %))
       (map hash)
       (map #(mod % m))))

(defn- add* [bloom x]
  (let [s (pr-str x)
	{:keys [m k f]} bloom]
    (reduce (fn [bs n] (.set #^BitSet bs n) bs)
	    f (indexes s m k))
    bloom))

(defn add [bloom x]
  (swap! bloom add* x)
  bloom)

(defn contains? [bloom x]
  (let [s (pr-str x)
	{:keys [m k f]} @bloom]
    (every? #(.get #^BitSet f %) (indexes s m k))))

(defn benchmark []
  ;; Inserting 1 mio. entries into a reasonably sized bloom filter
  ;; and retrieving them with a hit rate of 0.05. (Error rate of 0.1%)
  (let [n 100000
	_ (println (format "Insert %,d entries" n))
	m (* n 10)
	bt (create-bloom m n)]
    (time (reduce add bt (range n)))
    (println (format "Retrieving %,d entries" n))
    (let [lim (/ n 0.05)] ;; hit rate 0.05
      (time (doseq [x (range n)]
	      (contains? bt (Math/round (rand lim))))))
    (let [s (time (str (:f @bt)))]
      (println (format "Serialized size: %,d" (.length s))))))

