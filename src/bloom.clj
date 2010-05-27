(ns bloom
  (:import java.security.MessageDigest)
  (:import java.nio.charset.Charset)
  (:use [clojure.contrib.except :only [throw-if-not]])
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

(defn make-bloom
  "Create a Bloom filter with bit size M, number of (expected)
elements N and number of hash functions K. K can be calculated."
  [m k]
  (atom {:m m :k k :f (make-array Byte/TYPE (byte-size m))}))

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
  "Change BLOOM to contain X and return it.."
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

(defn match? [a b]
  (and (= (:m @a) (:m @b)) (= (:k @a) (:k @b))))

(defn union [a b & more]
  (throw-if-not (match? a b) "Bloom filters different, cannot union")
  (let [u (make-array Byte/TYPE (alength (:f @a)))
	a-bs (:f @a)
	b-bs (:f @b)]
    (doseq [n (range (alength (:f @a)))]
      (aset-byte u n (bit-or (aget a-bs n) (aget b-bs n))))
    (atom (assoc @a :f u))))
