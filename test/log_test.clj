(ns log-test
  (:use [log] :reload)
  (:use [clojure.test])
  (:require [clojure.contrib.io :as io]))

(derive ::collatz :log/debug)
(derive ::up ::collatz)
(derive ::down ::collatz)

(defn up [v]
  (log ::up "Up input: " v)
  (+ 1 (* 3 v)))

(defn down [v]
  (spy ::down (/ v 2)))

(defn collatz [v]
  (cond
    (= 1 v) 1
    (even? v) (recur (down v))
    :else (recur (up v))))

(deftest channel-test
  (let [func (fn [& args] args)]
    (log->fn ::test-channel func)
    (is (logged? ::test-channel))
    (is (not (logged? ::foo)))
    (log-clear-writers ::test-channel)
    (log->fn :test-key ::test-channel-b func)
    (is (logged? ::test-channel-b))
    (log-remove-writer :test-key ::test-channel-b)
    (is (not (logged? ::test-channel-b)))
    ))

(derive ::b ::a)
(derive ::c ::b)

(deftest console-log-test
  (log->console ::b)
  (let [a (with-out-str (log ::a "foo"))
        b (with-out-str (log ::b "foo"))
        c (with-out-str (log ::c "foo"))]
    (is (= a ""))
    (is (= b "b: foo\n"))
    (is (= c "c: foo\n"))))

(deftest file-log-test
  (log->file ::b "test-log")
  (log ::a "a")
  (log ::b "b stuff")
  (log ::c "c message")
  (log ::b :close)
  (let [lines (io/read-lines "test-log")
        b (first lines)
        c (second lines)]
    (is (= b "b: b stuff"))
    (is (= c "c: c message"))))

(defn log-test-fixture [f]
  (log-clear-all-writers)
  (f)
  (log-clear-all-writers))

(use-fixtures :each log-test-fixture)

(defn logjam-tests []
  (binding [*test-out* *out*]
    (run-tests 'log-test)))

