(ns
   #^{:author "Jeff Rose, borrowing code from clojure.contrib.logging"
      :doc "An experimental, task-based logging library."}
  logjam.core
  (:refer-clojure :exclude [println format])
  (:use clansi.core
        [clojure.contrib.core :only (dissoc-in)])
  (:require
    [clojure.contrib.trace :as trace]
    [clojure.java.io :as io]))

(defonce writers* (ref {}))
(defonce color?* (atom false))
(defonce direct-logging?* (atom true))
(defonce logging-agent* (agent nil))

(defn in-color [& [color]]
  (reset! color?* (if (false? color) false true)))

(defn default-channel-tree []
  (-> (make-hierarchy)
    (derive ::debug ::trace)
    (derive ::warn  ::debug)
    (derive ::error ::warn)
    (derive ::fatal ::error)
    (derive ::info  ::fatal)))

(defonce channel-tree* (ref (default-channel-tree)))

(defn reset-channels
  []
  (dosync (ref-set channel-tree* (default-channel-tree)))
  :reset)

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
  ([chan-key channel writer]
   (dosync (alter writers*
                  update-in [(chan-name channel)]
                  #(assoc % chan-key writer)))))

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
  [label args]
  (let [args (map #(if (nil? %) "nil" %) args)]
    (if (not= (first args) :close)
      (if @color?*
        (apply str
               (style "[" :red)
               label
               (style "]" :red) " "
               (interpose " " args))
        (apply str "[" label "] "
               (interpose " " args))))))

(defn- base-writer
  "A basic writer that uses expects an io/writer (java.io.Writer)."
  [^java.io.Writer writer]
  (fn [chan args]
    (if (= (first args) :close)
      (.close writer)
      (do
        (.write writer ^String (log-msg chan args))
        (.write writer "\n")
        (.flush writer)))))


(defn console-writer
  "Returns a basic console writer function."
  []
  (let [c-out *out*]
    (fn [chan args]
      (binding [*out* c-out]
        (clojure.core/println (log-msg chan args))))))

(defn console
  "Setup a log channel to output to the console.
    (log/console :data-importer)
  "
  ([channels]
   (console channels :console))
  ([channels chan-key]
   (let [channels (if (coll? channels)
                    channels
                    [channels])]
     (doseq [channel channels]
       (add-writer chan-key channel (console-writer))
       chan-key))))

(defn repl-writer
  [out-stream]
  (fn [chan args] (.println out-stream (log-msg chan args))))

(defn repl
  "Log output to the System/out.

  This works in the nailgun repl of vimclojure, but might be more
  generally useful elsewhere."
  ([channels]
   (repl channels :repl))
  ([channels chan-key]
   (let [channels (if (coll? channels)
                    channels
                    [channels])]
     (doseq [channel channels]
       (add-writer chan-key channel (repl-writer System/out))))))

(defn file-writer
  "Returns a file based writer function configured to write to the
  file located at path."
  [path]
  (base-writer (io/writer path :append true)))

(defn file
  ([channels path] (file channels path :file))
  ([channels path chan-key]
   (let [channels (if (coll? channels)
                    channels
                    [channels])
         names (map chan-name channels)]
     (doseq [ch-name names]
       (add-writer chan-key ch-name (file-writer path))
       chan-key))))

(defn remove-writer
  "Remove the writer registered with a channel-key for a channel."
  [channel chan-key]
  (let [channel (chan-name channel)]
    (dosync
      (alter writers* assoc channel
             (dissoc (get writers* channel {}) chan-key))
      (if (empty? (get writers* channel))
        (alter writers* dissoc channel)))))

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
         (w (name base-chan) args)) ; call the writers for this channel
       (doseq [parent (parents @channel-tree* channel)]
         (log-event base-chan parent args)))))) ; recurse up to the first parent

(defmacro println
  "Log a message to the given channel."
  [channel & args]
  `(if @direct-logging?*
     (log-event ~channel (list ~@args))
     (send-off logging-agent*
       (fn [_# c# & args#] (log-event c# args#))
       ~channel ~@args)))

(defmacro to [& args] `(println ~@args))

(defmacro format
  "Log a formatted message to the given channel."
  [channel fmt & args]
  `(if @direct-logging?*
     (log-event ~channel [(clojure.core/format ~fmt ~@args)])
     (send-off logging-agent*
       (fn [_# c# fmt# args#] (log-event c# [(apply clojure.core/format fmt# args#)]))
       ~channel ~fmt [~@args])))

(defmacro def-let
  "like let, but binds the expressions globally."
  [bindings & more]
  (let [let-expr (macroexpand `(let ~bindings))
        names-values (partition 2 (second let-expr))
        defs   (map #(cons 'def %) names-values)]
    (concat (list 'do) defs more)))

(defmacro spy
  "Evaluates expr and outputs the form and its result to a channel.
  Returns the result of expr."
  [channel expr]
  (if (= 'let (first expr))
    (let [bindings (second expr)
          body (drop 2 expr)
          let-expr (macroexpand `(let ~bindings))
          names-values (partition 2 (second let-expr))
          logs (mapcat (fn [[name value]]
                         `["\n  " (str (quote ~name) ":") ~value])
                       names-values)
          log (concat `(to ~channel "\nlet:") logs)]
      (concat (list 'let (vec bindings) log) body))
    `(let [a# ~expr]
       (to ~channel (str '~expr " => " a#)) a#)))

; TODO: Hook into c.c.trace functions, writing to a log channel
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
