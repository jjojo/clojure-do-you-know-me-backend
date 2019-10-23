(ns do-you-know-me.handler
  (:require [org.httpkit.server :refer :all]
    [clojure.test :refer :all]
    [clojure.data.json :as json]
    [do-you-know-me.game :refer [create-game]]))

(def clients (atom {}))

(defn handler [request]
  (with-channel request channel
    (swap! clients assoc channel true)
    (println channel "a user connected")
    (on-close channel (fn [status] (println "channel closed: " status)))
    (on-receive channel (fn [data] ;; echo it back
                          (println (json/read-str data :key-fn keyword))
                          (if (= (:action (json/read-str data :key-fn keyword)) "CREATE_GAME")
                            (send! channel (json/write-str (create-game)))
                            nil)))))
