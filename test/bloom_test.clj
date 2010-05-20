(ns bloom-test
  (:use [bloom] :reload-all)
  (:use [clojure.test])
  (:refer-clojure :exclude [hash contains?]))

(deftest k-test
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
  (is (= {:m 1 :n 2 :k 3 :f 0} (create-bloom 1 2 3)))
  (is (= {:m 100 :n 10 :k 7 :f 0} (create-bloom 100 10))))

(deftest bytes->num-test
  (are [x y] (= x (Long/toString (bytes->num (byte-array (map byte y))) 2))
       "0" [0]
       "11111111" [255]
       "1111111100000000" [255 0]
       "11110000" [240]
       "1111" [15]
       "111111110000000011110000111111110000111100000000" [255 0 240 255 15 0]))

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
  (let [b (-> (create-bloom 16 4 1)
	      (add "a"))]
    (is (= "10000" (Integer/toString (:f b) 2))))
  (let [b (-> (create-bloom 16 4 3)
	      (add "b"))]
    (is (= "100000000000101" (Integer/toString (:f b) 2)))))

(deftest add-multiple-test
  (let [b (create-bloom 48 0 3)
	b1 (reduce add b [1 2 3 4 5])]
;;    (is (= 21200912795920 (:f b1)))
    (is (every? identity (map #(contains? b1 %) [1 2 3 4 5])))
    (is (not-any? identity (map #(contains? b1 %) [6 7 8 9 10])))))

(deftest contains?-test
  (let [b (-> (create-bloom 64 8 4)
	       (add "a")
	       (add "b")
	       (add "c"))]
    (is (contains? b "a"))
    (is (contains? b "b"))
    (is (contains? b "c"))
    (is (not (contains? b "d")))
    (is (not (contains? b "e")))
    (is (not (contains? b "f")))))
