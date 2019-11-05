(ns do-you-know-me.handler
  (:require [org.httpkit.server :refer :all]
            [clojure.test :refer :all]
            [clojure.data.json :as json]
            [nano-id.core :refer [nano-id]]
            [do-you-know-me.game :refer [create-game
                                         add-player
                                         get-players
                                         set-username
                                         add-question-to-player
                                         get-game-questions
                                         set-player-ready
                                         start-game
                                         set-focus
                                         set-active-question
                                         answer
                                         correct-answer
                                         maybe-change-turn]]))

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

(defn start-game?
  [id]
  (if (every? :ready (-> (get-game-state id)
                         (get-players)))
    (update-game-state! (start-game (get-game-state id)) id)
    nil))


(defn handler [request]
  (with-channel request channel
                (println "a user connected")
                (on-close channel (fn [status]
                                    (println "channel closed: " status)
                                    (map (fn [key]
                                           (if (= (key @clients) channel)
                                             (swap! clients dissoc key)
                                             nil)
                                           (keys @clients)))))

                (on-receive channel (fn [data]
                                      (println (json/read-str data :key-fn keyword))
                                      (as-> (json/read-str data :key-fn keyword) data
                                            (case (:action data)
                                              "CREATE_GAME" (let [id (nano-id 4)]
                                                              (swap! clients assoc (keyword id) (conj channel))
                                                              (update-game-state! (create-game id) id)
                                                              (send-answer! channel "GAME_STATE" id))

                                              "GET_GAME_STATE" (do (cond
                                                                     (contains? data :playerId) (swap! clients assoc (keyword (:playerId data)) (conj channel))
                                                                     (contains? data :id) (swap! clients assoc (keyword (:id data)) (conj channel))
                                                                     :else nil)
                                                                   (broadcast-answer! "GAME_STATE" (:id data)))

                                              "GET_QUESTIONS" (send! channel (json/write-str {:type "QUESTIONS" :payload (get-game-questions)}))

                                              "JOIN_GAME" (let [player-id (nano-id 10)]
                                                            (swap! clients assoc (keyword player-id) (conj channel))
                                                            (update-game-state! (add-player (get-game-state (:id data)) player-id) (:id data))
                                                            (send! channel (json/write-str {:type "PLAYER_ID" :payload player-id}))
                                                            (broadcast-answer! "GAME_STATE" (:id data)))

                                              "SET_USERNAME" (do
                                                               (println "SET USERNAME RUNS")
                                                               (update-game-state! (set-username (get-game-state (:id data))
                                                                                                 (:playerId data)
                                                                                                 (:username data)) (:id data))
                                                               (broadcast-answer! "GAME_STATE" (:id data)))
                                              "ADD_QUESTION" (do
                                                               (println "IN ADD QUESTION !")
                                                               (update-game-state! (add-question-to-player (get-game-state (:id data))
                                                                                                           (:playerId data)
                                                                                                           (:questionId data)) (:id data))
                                                               (broadcast-answer! "GAME_STATE" (:id data)))

                                              "SET_PLAYER_READY" (do
                                                                   (println "IN SET READY!")
                                                                   (update-game-state! (set-player-ready (get-game-state (:id data))
                                                                                                         (:playerId data)
                                                                                                         (:ready data)) (:id data))
                                                                   (start-game? (:id data))
                                                                   (broadcast-answer! "GAME_STATE" (:id data)))

                                              "SET_FOCUS" (do (update-game-state! (set-focus (get-game-state (:id data))
                                                                                             (:playerId data)
                                                                                             (:questionId data)) (:id data))
                                                              (broadcast-answer! "GAME_STATE" (:id data)))

                                              "SET_ACTIVE_QUESTION" (do (update-game-state! (set-active-question
                                                                                              (get-game-state (:id data))
                                                                                              (:question data)) (:id data))
                                                                        (broadcast-answer! "GAME_STATE" (:id data)))

                                              "ANSWER" (do (update-game-state! (answer
                                                                                 (get-game-state (:id data))
                                                                                 (:playerId data)
                                                                                 (:answer data)) (:id data))
                                                           (broadcast-answer! "GAME_STATE" (:id data)))

                                              "CORRECT_ANSWER" (do (println "CORRECT ANSWER RUNS")
                                                                 (update-game-state! (-> (correct-answer (get-game-state (:id data))
                                                                                                           (:playerId data)
                                                                                                           (:correct data))
                                                                                           (maybe-change-turn)) (:id data))
                                                                   (println "WANTS TO SEND " (get-game-state (:id data)))
                                                                   (broadcast-answer! "GAME_STATE" (:id data)))
                                              "error")
                                            )
                                      ))))
