(ns
   #^{:author "Jeff Rose, borrowing code from clojure.contrib.logging"
      :doc "An experimental, task-based logging library."}
  log
  (:require
     [clojure.contrib.trace :as trace]
     [clojure.contrib.io :as io]))

(def direct-logging?* (atom true))
(def logging-agent* (agent nil))

(def channels* (ref {::debug {:console console-writer }}))
(def active-channels* (ref #{::debug}))

(derive ::debug ::trace)
(derive ::warn  ::debug)
(derive ::error ::warn)
(derive ::fatal ::error)
(derive ::info  ::fatal)

(def console-writer println)

(defn file-writer [path]
  (let [w (io/append-writer path)]
    #(.write w %)))

(defn log->file 
  ([channel path] (log->file (gensym) channel path))
  ([chan-key channel path]
   (dosync (alter channels* assoc channel 
                  (assoc (get channels* chan-key {}) 
                         chan-key (file-writer path))))))

(defn log-stop 
  [channel path]
  (dosync alter channels* assoc ))

(defn- log-tree [channel msg]
  (if-let [writers (get @channels* channel)]
    (doseq [[_ w] writers]
      (w msg))
    (doseq [chan (parents channel)]
      (log-tree chan msg))))

(defn log-channel-write [channel & args]
  (let [msg (apply str (interpose " " args))]
    (log-tree channel msg)))

(defmacro log
  "Log a message to the given channel."
  [channel & args]
  `(if @direct-logging?*
     (log-channel-write ~channel ~@args)
     (send-off logging-agent*
       (fn [_# c# & args#] (apply log-channel-write c# args#))
       ~channel ~@args)))

(def *SPY-CHANNEL* ::debug)

(defmacro spy
    "Evaluates expr and outputs the form and its result to the debug log; returns
      the result of expr."
    [expr]
    `(let [a# ~expr] (log *SPY-CHANNEL* (str '~expr " => " a#)) a#))

; Hook into c.c.trace functions, writing to a log channel
(defmacro trace-log
  "Outputs the trace to a log channel."
  [channel trace-symbols & form])

(def *old-std-streams* (ref nil))

(comment defn log-capture!
  "Captures System.out and System.err, redirecting all writes of those streams
  to :info and :error logging, respectively. The specified log-ns value will
  be used to namespace all redirected logging. NOTE: this will not redirect
  output of *out* or *err*; for that, use with-logs."
  [log-ns]
  (dosync
    (let [new-out (log-stream :info log-ns)
          new-err (log-stream :error log-ns)]
      ; don't overwrite the original values
      (if (nil? @*old-std-streams*)
        (ref-set *old-std-streams* {:out System/out :err System/err}))
      (System/setOut new-out)
      (System/setErr new-err))))

