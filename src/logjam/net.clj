(ns logjam.net
  (require [logjam :as log])
  (:import
    [java.net InetAddress InetSocketAddress SocketAddress 
     DatagramPacket DatagramSocket]
    [java.nio ByteBuffer CharBuffer]
    [java.nio.channels Channel DatagramChannel SelectionKey Selector
     ServerSocketChannel SocketChannel]
    [java.nio.charset Charset CharsetEncoder]))

(def MAX-LOG-EVENT-SIZE 1024)
(def SELECT-TIMEOUT 100)

(def charset (Charset/forName "UTF-8"))

(def ERROR (atom nil))

(defn- server-loop 
  [{:keys [listening? tcp-chan udp-chan selector tcp-clients] :as s}]
  (try
  (let [recv-buf  (ByteBuffer/allocate MAX-LOG-EVENT-SIZE)
        decoder (.newDecoder charset)]
    (while @listening?
      (try
        (.select selector SELECT-TIMEOUT)
        (doseq [k (.selectedKeys selector)]
          (.clear recv-buf)
          (let [chan (.channel k)]
            (cond
              (and (.isAcceptable k)
                   (= tcp-chan chan))
              (if-let [client (.accept tcp-chan)]
                (dosync (alter tcp-clients conj client)))
              (and (.isReadable k)
                   (= udp-chan chan))
              (let [client-addr (.receive udp-chan recv-buf)]
                (when (pos? (.position recv-buf))
                  (.flip recv-buf)
                  (let [recv-str (.toString (.decode decoder recv-buf))
                        {:keys [channel args]} (read-string recv-str)]
                (logjam/log-event channel args)))))))
        (catch java.io.IOException e
          (log/to :server "got IO error: " e)))))
    (catch Exception e
      (reset! ERROR e)
      (log/to :server "Got exception in server-loop: " 
              (let [err (java.io.StringWriter.)]
              (binding [*err* err]
                (.printStackTrace e))
                err)))))

(defn server [port]
  (let [listening? (atom true)
        addr (InetSocketAddress. port)
        tcp-chan (ServerSocketChannel/open)
        tcp-clients (ref #{})
        udp-chan (DatagramChannel/open)
        selector  (Selector/open)
        s {:listening? listening?
           :tcp-chan tcp-chan
           :tcp-clients tcp-clients
           :udp-chan udp-chan
           :selector selector}
       server-thread (Thread. #(server-loop s))]

    (-> tcp-chan (.socket) (.bind addr))
    (doto tcp-chan
      (.configureBlocking false)
      (.register selector SelectionKey/OP_ACCEPT))

    (-> udp-chan (.socket) (.bind addr))
    (doto udp-chan
      (.configureBlocking false)
      (.register selector SelectionKey/OP_READ))

    (.start server-thread)
    s))

(defn stop-server [{:keys [listening? tcp-chan udp-chan]}]
  (reset! listening? false)
  (.close tcp-chan)
  (.close udp-chan))

(defn udp-writer [host port]
  (let [addr (InetAddress/getByName host)
        sock (DatagramSocket.)
        encoder (.newEncoder charset)]
  (fn [chan args]
    (if (= :close (first args))
      (.close sock)
      (let [msg (with-out-str (prn {:channel chan
                                    :args args}))
            bytes (.getBytes msg)
            size (count bytes)]
        (.send sock (DatagramPacket. bytes size addr port)))))))
