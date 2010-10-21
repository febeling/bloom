(ns bloom.benchmark
  (:require [bloom :as bf])
  (:import (com.github.febeling NumericHelpers))
  (:import (java.util HashSet)))

(defn benchmark
  "Insert N (default 50'000) entries into a reasonably sized bloom
filter and retrieve them with a hit rate of 0.05. (Error rate of
0.1%)"
  ([] (benchmark (* 50 1000)))
  ([n]
     (println (format "Insert %,d entries" n))
     (let [m (* n 12)
	   k 4
	   bt (bf/make-bloom m k)]

       (println (format "Bloom filter k=%d" k))
       (time (reduce bf/add bt (map str (range n))))
       (println (format "Retrieving %,d entries" n))
       (let [lim (/ n 0.05)] ;; -> hit rate 0.05
	 (println (format "lim: %,d" lim))
	 (time (doseq [x (range n)]
		 (bf/contains? bt (str (Math/round (rand lim)))))))

       (println "hash map")
       (let [hs (HashSet. n)]
	 (time (reduce (fn [s e] (.add s e) s) hs (map str (range n))))
	 (println (format "Retrieving %,d entires" n))
	 (let [lim (/ n 0.05)] ;; -> hit rate 0.05
	   (time (doseq [x (range n)]
		   (.contains hs (str (Math/round (rand lim)))))))))))

