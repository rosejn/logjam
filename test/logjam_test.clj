(ns logjam-test
  (:use clojure.test
        clojure.stacktrace
        clojure.contrib.repl-utils)
  (:require :reload [logjam :as log])
  (:require [clojure.contrib.io :as io]))

(log/channel :collatz :debug)
(log/channel :up      :collatz)
(log/channel :down    :collatz)

(defn up [v]
  (log/to :up "Up input: " v)
  (+ 1 (* 3 v)))

(defn down [v]
  (log/spy :down (/ v 2)))

(defn collatz [v]
  (cond
    (= 1 v) 1
    (even? v) (recur (down v))
    :else (recur (up v))))

(deftest channel-test
  (let [func (fn [& args] args)]
    (log/add-writer :test-channel func)
    (is (log/written? :test-channel))
    (is (not (log/written? :foo)))
    (log/clear-writers :test-channel)
    (log/add-writer :test-key :test-channel-b func)
    (is (log/written? :test-channel-b))
    (log/remove-writer :test-channel-b :test-key)
    (is (not (log/written? :test-channel-b)))
    ))

(log/channel :b :a)
(log/channel :c :b)

(deftest console-log-test
  (log/console :b)
  (let [a (with-out-str (log/to :a "foo"))
        b (with-out-str (log/to :b "foo"))
        c (with-out-str (log/to :c "foo"))]
    (is (= a ""))
    (is (= b "b: foo\n"))
    (is (= c "c: foo\n")))
  
  (log/console :a :a-test-key)
  (let [a1 (with-out-str (log/to :a "foo"))
        _ (log/remove-writer :a :a-test-key)
        a2 (with-out-str (log/to :a "bar"))]
    (is (= a1 "a: foo\n"))
    (is (= a2 ""))))

(deftest file-log-test
  (log/file :b "test-log")
  (log/to :a "a")
  (log/to :b "b stuff")
  (log/to :c "c message")
  (log/to :b :close)
  (let [lines (io/read-lines "test-log")
        b (first lines)
        c (second lines)]
    (is (= b "b: b stuff"))
    (is (= c "c: c message"))))

(defn log-test-fixture [f]
;  (log/clear-all-writers)
  (f)
 ; (log/clear-all-writers)
  )

(use-fixtures :each log-test-fixture)

(defn logjam-tests []
  (binding [*test-out* *out*]
    (run-tests 'logjam-test)))

