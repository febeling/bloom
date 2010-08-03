(ns bloom
  (:use [clojure.contrib.except :only [throw-if-not]])
  (:import net.partow.GeneralHashFunctionLibrary)
  (:refer-clojure :exclude [contains?]))

;; TODO (intersection a b)
;; TODO (make-optimized-bloom element-count error-probability)
;; TODO add element count

(set! *warn-on-reflection* true)

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

(defstruct bloomfilter :m :k :f)

(defn make-bloom
  "Create a Bloom filter with bit size M, number of (expected)
elements N and number of hash functions K. K can be calculated."
  [m k]
  (atom (struct bloomfilter m k (make-array Byte/TYPE (byte-size m)))))

(defn abit-array-pos [byte-array i]
  (- (dec (alength byte-array))
     (int (/ i 8))))

(defn abit-bit-pos [byte-array i]
  (rem i 8))

;; CLOJURE 1.1

;; (defn byte
;;   "Coerce to byte"
;;   {:tag Byte
;;    :inline (fn [x] `(. clojure.lang.RT (byteCast ~x)))}
;;   [#^Number x] (. x (byteValue)))

(defn abit-set [byte-array i]
  (let [apos (abit-array-pos byte-array i)
	bpos (abit-bit-pos byte-array i)]
    (aset-byte byte-array
	       apos
	       (-> #^bytes byte-array
                   (aget #^Integer apos)
                   (bit-set bpos)
                   byte))
    byte-array))

;; CLOJURE 1.2

;; (defn byte
;;   "Coerce to byte"
;;   {:tag Byte
;;    :inline (fn [x] `(. clojure.lang.RT (byteCast ~x)))
;;    :added "1.0"}
;;   [^Number x] (clojure.lang.RT/byteCast x))

(defn byte-1
  "Coerce to byte"
  [#^Number x] (. x (byteValue)))

(defn abit-set [byte-array i]
  (let [apos (abit-array-pos byte-array i)
	bpos (abit-bit-pos byte-array i)]
    (aset-byte byte-array
	       apos
	       (-> #^bytes byte-array
                   (aget #^Integer apos)
                   (bit-set bpos)
                   (byte-1)))
    byte-array))

(defn abit-test [byte-array i]
  (. GeneralHashFunctionLibrary isBitSet byte-array i))

(defn bytes->num [bs]
  (->> bs
       (map #(bit-and 0xff %))
       (reduce #(bit-or (bit-shift-left %1 8) %2) 0)))

(defn indexes [s m k]
  (. GeneralHashFunctionLibrary indexes s m k))

(defn- add* [bloom s]
  (let [{:keys [m k f]} bloom]
    (reduce (fn [bs n] (abit-set bs n) bs)
	    f (indexes s m k))
    bloom))

(defn add [bloom #^String x]
  "Change BLOOM to contain X and return it."
  (swap! bloom add* x)
  bloom)

(defn contains? [bloom #^String s]
  (let [bf-map @bloom
	m (get bf-map :m)
	k (get bf-map :k)
	f (get bf-map :f)]
    (. GeneralHashFunctionLibrary contains f s m k)
;;    (every? (fn [n] (abit-test f n)) (indexes s m k))
    ))

(defn pack [bloom]
  (assoc @bloom :f (into [] (:f @bloom))))

(defn unpack [bloom]
  (let [f (into-array Byte/TYPE (map byte (:f bloom)))]
    (into (struct-map bloomfilter) (assoc bloom :f f))))

(defn serialize [bloom]
  (pr-str (pack bloom)))

(defn deserialize [s]
  (atom (unpack (read-string s))))

(defn match? [a b]
  (and (= (:m @a) (:m @b)) (= (:k @a) (:k @b))))

(defn union [a b & more]
  (throw-if-not (match? a b) "Bloom filters different, cannot union")
  (let [u-bs (make-array Byte/TYPE (alength (:f @a)))
	a-bs (:f @a)
	b-bs (:f @b)]
    (doseq [n (range (alength (:f @a)))]
      (aset-byte u-bs n (bit-or #^Byte (aget #^bytes a-bs #^Integer n)
				#^Byte (aget #^bytes b-bs #^Integer n))))
    (let [u (atom (assoc @a :f u-bs))]
      (if (empty? more)
	u
	(recur u (first more) (rest more))))))

(set! *warn-on-reflection* false)
