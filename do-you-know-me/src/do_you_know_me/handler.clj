(ns do-you-know-me.handler
  (:require [org.httpkit.server :refer :all]
            [clojure.test :refer :all]
            [clojure.data.json :as json]
            [nano-id.core :refer [nano-id]]
            [do-you-know-me.game :refer [create-game
                                         add-player
                                         get-players
                                         set-username]]))

(def clients (atom {}))
(def game-states (atom {}))

(defn get-game-state
  {:test (fn []
           (create-game "1234")
           (is (= (get-game-state "1234") (create-game "1234"))))}
  [id]
  ((keyword id) @game-states))

(defn update-game-state!
  [state id]
  (swap! game-states assoc (keyword id) state))


(defn send-answer!
  [channel action id]
  (send! channel (json/write-str {:type    action
                                  :payload (get-game-state id)})))

(defn get-players-in-game
  [id]
  (get-players (get-game-state id)))


(defn get-player-channel
  [player-id]
  ((keyword player-id) @clients))


(defn send-to-game!
  [action id]
  (send! ((keyword id) @clients)
         (json/write-str {:type    action
                          :payload (get-game-state id)})))


(defn broadcast-answer!
  [action id]
  (send-to-game! action id)
  (doall (map (fn [player]
                (send!
                  (get-player-channel (:id player))
                  (json/write-str {:type    action
                                   :payload (get-game-state id)})))
              (get-players-in-game id))))


(defn handler [request]
  (with-channel request channel
                (println "a user connected")
                (on-close channel (fn [status] (println "channel closed: " status))) ; remove channel from clients on close
                (on-receive channel (fn [data]
                                      (println (json/read-str data :key-fn keyword))
                                      (as-> (json/read-str data :key-fn keyword) data
                                            (case (:action data)
                                              "CREATE_GAME" (let [id (nano-id 4)]
                                                              (swap! clients assoc (keyword id) (conj channel))
                                                              (update-game-state! (create-game id) id)
                                                              (send-answer! channel "GAME_STATE" id))

                                              "GET_GAME_STATE" (send-answer! channel "GAME_STATE" (:id data))

                                              "JOIN_GAME" (let [player-id (nano-id 10)]
                                                            (swap! clients assoc (keyword player-id) (conj channel))
                                                            (update-game-state! (add-player (get-game-state (:id data)) player-id) (:id data))
                                                            (broadcast-answer! "GAME_STATE" (:id data))
                                                            (send! channel (json/write-str {:type "PLAYER_ID" :payload player-id})))

                                              "SET_USERNAME" (do
                                                               (println "IN SET USERNAME !")
                                                               (println (set-username (get-game-state (:id data))
                                                                                      (:playerId data)
                                                                                      (:username data)))
                                                               (update-game-state! (set-username (get-game-state (:id data))
                                                                                               (:playerId data)
                                                                                               (:username data)) (:id data))
                                                               (broadcast-answer! "GAME_STATE" (:id data)))
                                              "error")
                                            )
                                      ))))
