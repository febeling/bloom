(ns bloom
  (:import java.security.MessageDigest)
  (:import java.nio.charset.Charset)
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
      #^Double (* (Math/log 2.0))
      (Math/round)))

(defn byte-size [bits]
  (int (Math/ceil (float (/ bits 8)))))

(defn create-bloom
  "Create a Bloom filter with bit size M, number of (expected)
elements N and number of hash functions K. K can be calculated."
  ([m n k1]
     (atom {:m m :n n :k k1 :f (make-array Byte/TYPE (byte-size m))}))
  ([m n] (create-bloom m n (optimal-k m n))))

(defn abit-array-pos [byte-array i]
  (dec (- (alength byte-array) (int (/ i 8)))))

(defn abit-bit-pos [byte-array i]
  (rem i 8))

(defn abit-set [byte-array i]
  (let [apos (abit-array-pos byte-array i)
	bpos (abit-bit-pos byte-array i)]
    (aset-byte byte-array
	       apos
	       (-> #^bytes byte-array (aget #^Integer apos) (bit-set bpos) byte))
    byte-array))

(defn abit-test [byte-array i]
  (let [apos (abit-array-pos byte-array i)
	bpos (abit-bit-pos byte-array i)]
    (bit-test (aget #^bytes byte-array #^Integer apos) bpos)))
    
(defn bytes->num [bs]
  (->> bs
       (map #(bit-and 0xff %))
       (reduce #(bit-or (bit-shift-left %1 8) %2) 0)))

(defn hashnum [x]
  (let [bs (.getBytes #^String x charset)]
    (bytes->num (. #^MessageDigest message-digest digest bs))))

(defn indexes [s m k]
  (->> (range k)
       (map #(str s %))
       (map hashnum)
       (map #(mod % m))))

(defn- add* [bloom x]
  (let [s (pr-str x)
	{:keys [m k f]} bloom]
    (reduce (fn [bs n] (abit-set bs n) bs)
	    f (indexes s m k))
    bloom))

(defn add [bloom x]
  (swap! bloom add* x)
  bloom)

(defn contains? [bloom x]
  (let [s (pr-str x)
	{:keys [m k f]} @bloom]
    (every? #(abit-test f %) (indexes s m k))))

(defn serialize [bloom]
  (let [num (into [] (:f @bloom))]
    (pr-str (assoc @bloom :f num))))

(defn deserialize [s]
  (let [bloom (read-string s)
	f (into-array Byte/TYPE (map byte (:f bloom)))]
    (atom (assoc bloom :f f))))
