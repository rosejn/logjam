(ns
   #^{:author "Jeff Rose, borrowing code from clojure.contrib.logging"
      :doc "An experimental, task-based logging library."}
  logjam
  (:require
    [clojure.contrib.trace :as trace]
    [clojure.contrib.io :as io]))

(defonce writers* (ref {}))

(defonce direct-logging?* (atom true))
(defonce logging-agent* (agent nil))

(defn default-channel-tree []
  (-> (make-hierarchy)
    (derive ::debug ::trace)
    (derive ::warn  ::debug)
    (derive ::error ::warn)
    (derive ::fatal ::error)
    (derive ::info  ::fatal)))

(defonce channel-tree* (ref (default-channel-tree)))

(defn- chan-name [kw]
  (keyword "logjam" (name kw)))

(defn channel
  "Create a new log channel deriving from a parent channel, which defaults to :info
  if no parent channel is specified."
  ([child] (channel child :info))
  ([child parent]
   (dosync (ref-set channel-tree*
                    (derive @channel-tree* (chan-name child) (chan-name parent))))))

(defn add-writer 
  ([channel fun] (add-writer (gensym) channel fun))
  ([chan-key channel fun]
   (dosync (alter writers* assoc (chan-name channel)
                  (assoc (get @writers* chan-key {})
                         chan-key
                         fun)))))

(defn written? [channel]
  (let [channel (chan-name channel)]
    (not (nil?
           (some #(or (= channel %)
                      (isa? @channel-tree* channel %))
                 (keys @writers*))))))

(defn- log-msg
  "Create a standard log message string."
  [channel args]
  (if (not= (first args) :close)
    (apply str (name channel) ": " (interpose " " args))))

(defn- console-writer []
  (fn [chan args]
    (println (log-msg chan args))))

(defn- file-writer [path]
  (let [w (io/writer path)]
    (fn [chan args]
      (if (= (first args) :close)
        (.close w)
        (do
          (.write w (log-msg chan args))
          (.write w "\n"))))))

(defn console
  ([channel]
   (console (gensym) channel))
  ([chan-key channel]
   (add-writer chan-key channel (console-writer))
   chan-key))

(defn remove-writer [chan-key channel]
  (let [channel (chan-name channel)]
    (dosync
      (alter writers* assoc channel
             (dissoc (get writers* channel {}) chan-key))
      (if (empty? (get writers* channel))
        (alter writers* dissoc channel)))))

(defn file
  ([channel path] (file (gensym) channel path))
  ([chan-key channel path]
   (let [channel (chan-name channel)]
     (add-writer chan-key channel (file-writer path))
     chan-key)))

(defn clear-writers
  [channel]
  (let [channel (chan-name channel)]
    (dosync (alter writers* dissoc channel))))

(defn clear-all-writers []
  (dosync (ref-set writers* {})))

; TODO: This should probably be done using recur so we can handle any
; depth of hierarchy...
(defn log-event
  "Used internally to determine all channels that a log event needs to be 
  published too."
  ([channel args] 
     (log-event channel channel args))
  ([base-chan channel args]
   (let [channel (chan-name channel)]
     (if-let [writers (get @writers* channel)]
       (doseq [[_ w] writers]
         (w base-chan args)) ; call the writers for this channel
       (doseq [parent (parents @channel-tree* channel)]
         (log-event base-chan parent args)))))) ; recurse up to the first parent

(defmacro to 
  "Log a message to the given channel."
  [channel & args]
  `(if @direct-logging?*
     (log-event ~channel '~args)
     (send-off logging-agent*
       (fn [_# c# & args#] (log-event c# args#))
       ~channel ~@args)))

(defmacro spy
    "Evaluates expr and outputs the form and its result to the debug log; returns
      the result of expr."
    [channel expr]
    `(let [a# ~expr] (to ~channel (str '~expr " => " a#)) a#))

; Hook into c.c.trace functions, writing to a log channel
(defmacro trace
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

