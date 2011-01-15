(ns logjam.net-test
  (:use clojure.test
        clojure.stacktrace
        clojure.contrib.repl-utils)
  (:require [logjam.core :as log]
            [logjam.net :as net-log]))

(def TEST-PORT 5656)

(log/channel :foo :debug)
(log/channel :bar :foo)

(deftest basic-net-test
  (let [s (net-log/server TEST-PORT)]
    (try
      (log/add-writer :foo (net-log/udp-writer "localhost" TEST-PORT))
      (log/console :foo)
      (let [a (with-out-str
                (log/to :foo "foo")
                (log/to :bar "foo")
                (Thread/sleep 200))]
        (is (= a "[foo] foo\n[bar] foo\n")))
      (finally
        (net-log/stop-server s)))))

(defn net-tests []
  (binding [*test-out* *out*]
    (run-tests 'logjam.net-test)))
