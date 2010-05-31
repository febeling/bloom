(ns bloom.benchmark
  (:require [bloom :as bf])
  (:import (java.util HashSet)))

(defn benchmark
  "Inserting N entries into a reasonably sized bloom filter and
retrieving them with a hit rate of 0.05. (Error rate of 0.1%)"
  ([] (benchmark (* 100 1000)))
  ([n]
     (println (format "Insert %,d entries" n))
     (let [m (* n 100)
	   k 2
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
