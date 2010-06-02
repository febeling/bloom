(ns bloom.benchmark
  (:require [bloom :as bf])
  (:import (net.partow GeneralHashFunctionLibrary))
  (:import (java.util HashSet)))

(defn benchmark
  "Inserting N entries into a reasonably sized bloom filter and
retrieving them with a hit rate of 0.05. (Error rate of 0.1%)"
  ([] (benchmark (* 100 1000)))
  ([n]
     (println (format "Insert %,d entries" n))
     (let [m (* n 100)
	   k 4
	   bt (bf/make-bloom m k)]

       (println (format "bloom filter k=%d" k))
       (time (reduce bf/add bt (range n)))
       (println (format "Retrieving %,d entries" n))
       (let [lim (/ n 0.05)] ;; -> hit rate 0.05
	 (time (doseq [x (range n)]
		 (bf/contains? bt (Math/round (rand lim))))))

       (println "hash map")
       (let [hs (HashSet. n)]
	 (time (reduce (fn [s e] (.add s e) s) hs (range n)))
	 (println (format "Retrieving %,d entires" n))
	 (let [lim (/ n 0.05)] ;; -> hit rate 0.05
	   (time (doseq [x (range n)]
		   (.contains hs (Math/round (rand lim))))))))))

(defn hashes []
  (let [x 100000]
    (dotimes [i 10]
      (println "--- Round " i)
      (time (dotimes [n x] (. GeneralHashFunctionLibrary APHash "hello hash")))
      (time (dotimes [n x] (. GeneralHashFunctionLibrary BKDRHash "hello hash")))
      (time (dotimes [n x] (. GeneralHashFunctionLibrary BPHash "hello hash")))
      (time (dotimes [n x] (. GeneralHashFunctionLibrary DEKHash "hello hash")))
      (time (dotimes [n x] (. GeneralHashFunctionLibrary DJBHash "hello hash")))
      (time (dotimes [n x] (. GeneralHashFunctionLibrary ELFHash "hello hash")))
      (time (dotimes [n x] (. GeneralHashFunctionLibrary FNVHash "hello hash")))
      (time (dotimes [n x] (. GeneralHashFunctionLibrary JSHash "hello hash")))
      (time (dotimes [n x] (. GeneralHashFunctionLibrary PJWHash "hello hash")))
      (time (dotimes [n x] (. GeneralHashFunctionLibrary RSHash "hello hash")))
      (time (dotimes [n x] (. GeneralHashFunctionLibrary SDBMHash "hello hash"))))))
