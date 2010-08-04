(defproject bloom "0.4.3-SNAPSHOT"
  :description "Bloom filter implementation in Clojure"
  :dependencies [[org.clojure/clojure "1.2.0-RC1"]
                 [org.clojure/clojure-contrib "1.2.0-RC1"]]
  :dev-dependencies [[lein-javac "1.2.1-SNAPSHOT"]
                     [swank-clojure "1.2.1"]]
  :java-source-path "java"
  :compile-path "classes")
