(ns do-you-know-me.handler
  (:require [org.httpkit.server :refer :all]
            [clojure.test :refer :all]
            [clojure.data.json :as json]
            [nano-id.core :refer [nano-id]]
            [do-you-know-me.game :refer [create-game
                                         get-game-state
                                         add-player]]))

(def clients (atom {}))
(def game-states (atom {}))

(defn send-and-update-game-state-atom!
  [state channel action id]
  (println action id)
  (swap! game-states assoc (keyword id) state)
  (send! channel (json/write-str {:type    action
                                  :payload ((keyword id) @game-states)})))


(defn handler [request]
  (with-channel request channel
                (swap! clients assoc channel true)
                (println channel "a user connected")
                (on-close channel (fn [status] (println "channel closed: " status)))
                (on-receive channel (fn [data]
                                      (println (json/read-str data :key-fn keyword))
                                      (as-> (json/read-str data :key-fn keyword) data
                                            (case (:action data)
                                              "CREATE_GAME" (let [id (nano-id 4)]
                                                              (send-and-update-game-state-atom!
                                                              (create-game id)
                                                              channel
                                                              "GAME_STATE"
                                                              id))
                                              ;(send! channel (json/write-str {:type    "GAME_STATE"
                                              ;                                              :payload (create-game (nano-id 4))}))
                                              "GET_GAME_STATE" (send-and-update-game-state-atom!
                                                                 ((keyword (:id data)) @game-states)
                                                                 channel
                                                                 (:id data)
                                                                 "GAME_STATE")
                                              ;(send! channel (json/write-str {:type    "GAME_STATE"
                                              ;                                                 :payload (get-game-state (get data :id))}))
                                              "JOIN_GAME" (fn []
                                                            (add-player (get-game-state (get data :id)) (nano-id 10))
                                                            (send! channel (json/write-str {:type    "GAME_STATE"
                                                                                            :payload (get-game-state (get data :id))})))
                                              "error")
                                            )
                                      ))))
