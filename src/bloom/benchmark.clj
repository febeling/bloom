(ns bloom.benchmark
  (:require [bloom :as bf]))

(defn benchmark []
  "Inserting N entries into a reasonably sized bloom filter and
retrieving them with a hit rate of 0.05. (Error rate of 0.1%)"
  (let [n 100000
	_ (println (format "Insert %,d entries" n))
	m (* n 100)
	bt (bf/create-bloom m n 2)]
    (time (reduce bf/add bt (range n)))
    (println (format "Retrieving %,d entries" n))
    (let [lim (/ n 0.05)] ;; -> hit rate 0.05
      (time (doseq [x (range n)]
	      (bf/contains? bt (Math/round (rand lim))))))
    (let [s (time (serialize (:f @bt)))]
      (println (format "Serialized size: %,dk" (/ (.length s) 1024))))))
