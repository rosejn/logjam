(ns net-test
  (:use clojure.test
        clojure.stacktrace
        clojure.contrib.repl-utils)
  (:require :reload [logjam :as log]
            logjam.net))

(def TEST-PORT 5656)

(log/channel :foo :debug)

(deftest basic-net-test
  (let [s (logjam.net/server TEST-PORT)]
    (try
      (log/add-writer :foo (logjam.net/udp-writer "localhost" TEST-PORT))
      (log/console :foo)
      (let [a (with-out-str 
                (log/to :foo "foo")
                (Thread/sleep 200))]
        (is (= a "foo: foo\nfoo: foo\n")))
      (finally
        (logjam.net/stop-server s)))))

(defn net-tests []
  (binding [*test-out* *out*]
    (run-tests 'net-test)))
