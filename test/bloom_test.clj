(ns bloom-test
  (:use [bloom] :reload-all)
  (:use [clojure.test])
  (:refer-clojure :exclude [hash contains?]))

(deftest optimal-k-test
  ;; Expectations take from:
  ;; http://code.google.com/p/java-bloomfilter/source/browse/trunk/test/com/skjegstad/utils/BloomFilterTest.java
  (are [k m n] (= k (optimal-k m n))
       1 2 1
       2 3 1
       3 4 1
       3 5 1
       4 6 1
       5 7 1
       6 8 1
       6 9 1
       7 10 1
       8 11 1
       8 12 1))

(deftest create-bloom-test
  (let [bf @(create-bloom 100 10)]
    (is (= 100 (:m bf)))
    (is (= 10 (:n bf)))
    (is (= 7 (:k bf)))
    (is (= (:m bf) (alength (:f bf))))))

(defn bit-str [bs]
  (Long/toString (bytes->num (byte-array (map byte bs))) 2))

(deftest bytes->num-test
  (are [x y] (= x (bit-str y))
       "0" [0]
       "11111111" [255]
       "1111111100000000" [255 0]
       "11110000" [240]
       "1111" [15]
       "111111110000000011110000111111110000111100000000" [255 0 240 255 15 0]))

(deftest abit-set-test
  (let [ba (make-array Byte/TYPE 3)]
	(are [x y] (= x (bit-str y))
	     "0" ba
	     "1" (abit-set ba 0)
	     "11" (abit-set ba 1)
	     "10000011" (abit-set ba 7)
	     "1000000010000011" (abit-set ba 15)
	     "100000001000000010000011" (abit-set ba 23))))

(deftest abit-test-test
  (let [ba (-> (make-array Byte/TYPE 2)
	       (abit-set 0)
	       (abit-set 2)
	       (abit-set 4)
	       (abit-set 8)
	       (abit-set 10))]
	(are [x y] (is (= x (abit-test ba y)))
	     true 0
 	     true 2
	     false 1
 	     true 4
 	     true 8
 	     true 10
 	     false 3
 	     false 5
 	     false 7
 	     false 9)))

(deftest hash-test
  (are [x s] (= x (hash s))
       975987071262755080377722350727279193143145743181  "hello"
       708652540093010131229728076251103780790279585603  "world"
       1245845410931227995499360226027473197403882391305 ""))

(deftest indexes-test
  (are [x ps] (= x (apply indexes ps))
       [3 1 4] ["a" 10 3]
       [34 30 68 2 42] ["b" 100 5])
  (let [m 20
	k 5]
    (is (every? #(< % m) (indexes "x" m k)))
    (is (= k (count (indexes "x" m k))))))

(deftest add-test
  (let [b (create-bloom 16 4 1)]
    (add b "a")
    (is (= "10000" (bit-str (:f @b)))))
  (let [b (create-bloom 16 4 3)]
    (add b "b")
    (is (= "100000000000101" (bit-str (:f @b))))))

(deftest add-multiple-test
  (let [b (create-bloom 48 0 3)]
    (reduce add b [1 2 3 4 5])
    (is (= "100110100100000111000111000000100100100010000" (bit-str (:f @b))))
    (is (every? identity (map #(contains? b %) [1 2 3 4 5])))
    (is (not-any? identity (map #(contains? b %) [6 7 8 9 10])))))

(deftest contains?-test
  (let [b (create-bloom 64 8 4)]
    (add b "a")
    (add b "b")
    (add b "c")
    (is (contains? b "a"))
    (is (contains? b "b"))
    (is (contains? b "c"))
    (is (not (contains? b "d")))
    (is (not (contains? b "e")))
    (is (not (contains? b "f")))))
