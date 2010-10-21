(defproject bloom "0.5.0-SNAPSHOT"
  :description "Bloom filter implementation in Clojure"
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]
		 [com.github.febeling/general-hash-functions "1.1.0"]]
  :dev-dependencies [[lein-javac "1.2.1-SNAPSHOT"]
                     [swank-clojure "1.2.1"]]
  :hooks [leiningen.hooks.javac]
  :source-path "src"
  :java-source-path "java"
  :compile-path "build/classes")
