(ns
   #^{:author "Jeff Rose, borrowing code from clojure.contrib.logging"
      :doc "An experimental, task-based logging library."}
  logjam
  (:require
    [clojure.contrib.trace :as trace]
    [clojure.contrib.io :as io]
    [clojure.contrib.server-socket :as sock])
  (:import
    [java.net Socket]))

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
  "Create a new log channel deriving from a parent channel, which 
  defaults to :info if no parent channel is specified."
  ([child] (channel child :info))
  ([child parent]
   (dosync (ref-set channel-tree*
                    (derive @channel-tree* 
                            (chan-name child) 
                            (chan-name parent))))))

(defn add-writer 
  "Add a log writer to a log channel."
  ([channel writer] 
   (add-writer (gensym) channel writer))
  ([chan-key channel writer]
   (dosync (alter writers* assoc (chan-name channel)
                  (assoc (get @writers* chan-key {})
                         chan-key
                         writer)))))

(defn written? 
  "Are there any writers registered for this channel?"
  [channel]
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

(defn- console-writer 
  "Returns a basic console writer function."
  []
  (fn [chan args]
    (println (log-msg chan args))))

(defn- base-writer 
  "A basic writer that uses expects an io/writer (java.io.Writer)."
  [writer]
  (fn [chan args]
    (if (= (first args) :close)
      (.close writer)
      (do
        (.write writer (log-msg chan args))
        (.write writer "\n")))))

(defn- file-writer 
  "Returns a file based writer function configured to write to the
  file located at path."
  [path]
  (base-writer (io/writer path)))

(defn- socket-writer
  [host port]
  (let [writer (io/writer (Socket. host port))]
    (fn [chan args]
      (.write writer (prn-str {:type :logjam-msg 
                               :channel chan 
                               :args args})))))

(defn console
  "Setup a log channel to output to the console.  Optionally accepts
  a channel key, which can be used to remove this console writer
  in the future.
  
  (log/console :data-importer)
  (log/console :"

  ([channel]
   (console channel (gensym)))
  ([channel chan-key]
   (add-writer chan-key channel (console-writer))
   chan-key))

(defn remove-writer 
  "Remove the writer registered with a channel-key for a channel."
  [channel chan-key]
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

(defn socket
  ([channel host port] (socket channel host port (gensym)))
  ([channel host port chan-key]
   (let [channel (chan-name channel)]
     (add-writer chan-key channel (socket-writer host port))
     chan-key)))

(defn clear-writers
  [channel]
  (let [channel (chan-name channel)]
    (dosync (alter writers* dissoc channel))))

(defn clear-all-writers []
  (dosync (ref-set writers* {})))

; TODO: This should probably be done using recur so we can handle any
; depth of hierarchy... (Although no sane logging system would have
; more than a couple levels.)
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

(def LOG-SERVER-PORT 4242)

(defn- log-server-handler [running?]
  (fn [in out]
    (binding [*in* (io/reader in)
              *out* (io/writer out)]
      (try (loop [msg (read-line)]
             (println "got msg: " msg)
             (when (and @running? msg)
               (log-event (:channel msg) (:args msg))
               (recur (read-line))))
        (finally 
          (.close *in*)
          (.close *out*))))))

(defn server 
  ([] (server LOG-SERVER-PORT))
  ([port]
   (let [running? (atom true)]
     (sock/create-server (Integer. port) 
                         (log-server-handler running?))
     (fn [] (reset! running? false)))))

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

