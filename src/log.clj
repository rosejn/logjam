(ns
   #^{:author "Jeff Rose, borrowing code from clojure.contrib.logging"
      :doc "An experimental, task-based logging library."}
  log
  (:require
     [clojure.contrib.trace :as trace]
     [clojure.contrib.io :as io]))

(def writers* (ref {}))

(def direct-logging?* (atom true))
(def logging-agent* (agent nil))

(derive ::debug ::trace)
(derive ::warn  ::debug)
(derive ::error ::warn)
(derive ::fatal ::error)
(derive ::info  ::fatal)

(defn log->fn
  ([channel fun] (log->fn (gensym) channel fun))
  ([chan-key channel fun]
   (dosync (alter writers* assoc channel
                  (assoc (get @writers* chan-key {}) 
                         chan-key 
                         fun)))))

(defn logged? [channel]
  (not (nil? 
         (some #(or (=  channel %) 
                    (isa? channel %)) 
               (keys @writers*)))))

(defn- log-msg 
  "Create a standard log message string."
  [channel args]
  (apply str (name channel) ": " (interpose " " args)))

(def console-writer (fn [channel args] (println (log-msg channel args))))

(defn log->console
  ([channel] (log->console (gensym) channel))
  ([chan-key channel]
   (log->fn chan-key channel console-writer)
   chan-key))

(defn- file-writer [path]
  (let [w (io/append-writer path)]
    #(.write w %)))

(defn log->file
  ([channel path] (log->file (gensym) channel path))
  ([chan-key channel path]
   (log->fn chan-key channel (file-writer path))
   chan-key)) 

(defn log-stop
  [channel path]
  (dosync alter writers* assoc ))

(defn- log-tree [channel msg]
  (if-let [writers (get @writers* channel)]
    (doseq [[_ w] writers]
      (w msg))
    (doseq [chan (parents channel)]
      (log-tree chan msg))))

(defn log-channel-write [channel & args]
  (log-tree channel (log-msg channel args)))

(defmacro log
  "Log a message to the given channel."
  [channel & args]
  `(if @direct-logging?*
     (log-channel-write ~channel ~@args)
     (send-off logging-agent*
       (fn [_# c# & args#] (apply log-channel-write c# args#))
       ~channel ~@args)))

(defmacro spy
    "Evaluates expr and outputs the form and its result to the debug log; returns
      the result of expr."
    [channel expr]
    `(let [a# ~expr] (log ~channel (str '~expr " => " a#)) a#))

; Hook into c.c.trace functions, writing to a log channel
(defmacro trace-log
  "Outputs the trace to a log channel."
  [channel trace-symbols & form])

;(def *old-std-streams* (ref nil))

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

