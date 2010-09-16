(ns logjam.test.core
  (:use [log] :reload)
  (:use [clojure.test]))

(defn up [v] 
  (log ::up "Up input: " v)
 (+ 1 (* 3 v)))

(defn down [v] 
 (/ v 2))

(defn collatz [v]
  (cond
    (= 1 v) 1
    (even? v) (recur (down v))
    :else (recur (up v))))

(deftest replace-me ;; FIXME: write
  (is false "No tests have been written."))
