(ns do-you-know-me.game
  (:require [clojure.test :refer :all]
            [do-you-know-me.QUESTIONS :refer [questions]]
            [do-you-know-me.utils :refer [in?]]))

(def game-states (atom {}))

(def emojis ["🐹" "🐶" "🐱" "🦊" "🐻" "🐯" "🦁" "🐸" "🐵" "🐥"])
(def colors ["#f368e0" "#ff9f43" "#ee5253" "#ee5253" "#10ac84" "#5f27cd" "#795548" "#8BC34A"])


(defn create-game
  {:test (fn []
           (is (= (create-game "1234") (:1234 @game-states)))
           (is (= (->> (create-game "1234")
                       (:title))
                  "Do you know me? 🤔")))}
  [id]
  (swap! game-states assoc (keyword id) {:id          id
                                         :title       "Do you know me? 🤔"
                                         :gameStarted false
                                         :players     {}})
  ((keyword id) @game-states))


(defn get-game-state
  {:test (fn []
           (create-game "1234")
           (is (= (get-game-state "1234") (create-game "1234"))))}
  [id]
  ((keyword id) @game-states))


(defn get-player
  {:test (fn []
           (declare add-player)
           (is (= (:id (get-player (-> (create-game "1234")
                                       (add-player "p1"))
                                   "p1"))
                  "p1")))}
  [state id]
  (get-in state [:players (keyword id)]))


(defn get-players
  {:test (fn []
           (is (= (get-players (create-game "1234")) ()))
           (is (= (count (get-players (-> (create-game "1234")
                                          (add-player "p1")
                                          (add-player "p2"))))
                  2)))}
  [state]
  (map (fn [key]
         (get-player state key))
       (keys (:players state))))


(defn get-game-questions
  {:test (fn []
           (is (= (count (get-game-questions))
                  36)))}
  []
  (sort-by (juxt :points :id) (map (fn [key]
                                     (key questions))
                                   (keys questions))))


(defn get-coll-players-key
  {:test (fn []
           (is (= (count (-> (create-game "1234")
                             (add-player "p1")
                             (add-player "p2")
                             (get-coll-players-key :emoji)))
                  2)))}
  [state key]
  (map key (get-players state)))


(defn pick-unique-key
  {:test (fn []
           (let [state (create-game "1234")]
             (is (= (:color (pick-unique-key (add-player state "p1") colors :color))
                    (:color (get-player state "p1"))))
             (is (= (:emoji (pick-unique-key (add-player state "p1") emojis :emoji))
                    (:emoji (get-player state "p1"))))))}
  [state coll key]
  (rand-nth (filter
              (fn [item]
                (not (some #(= item %) (get-coll-players-key state key))))
              coll)))

;(defn update-player
;  [state, id]
;  (update-in state [:players (get-player state id)]))

(defn add-player
  {:test (fn []
           (is (= (get-in (add-player (create-game "1234") "p1") [:players :p1 :id]) "p1"))
           (is (= (keys (:players (-> (create-game "1234")
                                      (add-player "p1")
                                      (add-player "p2"))))
                  '(:p1 :p2))))}
  [state id]
  (assoc-in state
            [:players (keyword id)]
            {:username  "unknown"
             :id        id
             :emoji     (pick-unique-key state emojis :emoji)
             :color     (pick-unique-key state colors :color)
             :questions []
             :ready     false}))


(defn set-username
  {:test (fn []
           (is (= (get-in (set-username
                            (-> (create-game "1234")
                                (add-player "p1"))
                            "p1"
                            "Kalle") [:players :p1 :username])
                  "Kalle")))}
  [state, id, username]
  (assoc-in state [:players (keyword id) :username] username))


(defn add-question-to-player
  {:test (fn []
           (is (= (first (get-in (add-question-to-player
                                   (-> (create-game "1234")
                                       (add-player "p1"))
                                   "p1"
                                   {:id 1 :question "vad heter du?"}) [:players :p1 :questions]))
                  {:id       1
                   :question "vad heter du?"
                   :focus    false
                   :playerId "p1"})))}
  [state, id, question]
  (assoc-in state
            [:players (keyword id) :questions]
            (conj (get-in state
                          [:players (keyword id) :questions])
                  (-> (assoc question :focus false)
                      (assoc :playerId id)))))


(defn set-player-ready
  {:test (fn []
           (is (= (get-in (set-player-ready
                            (-> (create-game "1234")
                                (add-player "p1"))
                            "p1"
                            true) [:players :p1 :ready])
                  true)))}
  [state, id, ready]
  (assoc-in state [:players (keyword id) :ready] ready))


(defn start-game
  {:test (fn []
           (is (= (:gameStarted (start-game (-> (create-game "1234")
                                                (add-player "p1")))) true))
           (is (not= (:turn (start-game (-> (create-game "1234")
                                            (add-player "p1")
                                            (add-player "p2")))) nil)))}
  [state]
  (as-> (assoc state :gameStarted true) $
        (assoc $ :turn (:id (rand-nth (get-players $))))))


(defn set-focus
  {:test (fn []
           (is (= (:id (first (get-in (set-focus (-> (create-game "1234")
                                                     (add-player "p1")
                                                     (add-question-to-player "p1" {:id 1 :question "test question?"}))
                                                 "p1"
                                                 1) [:players :p1 :questions])))
                  1))
           (is (= (:id (second (get-in (set-focus (-> (create-game "1234")
                                                      (add-player "p1")
                                                      (add-question-to-player "p1" {:id 1 :question "test question?"})
                                                      (add-question-to-player "p1" {:id 2 :question "test question?"}))
                                                  "p1"
                                                  2) [:players :p1 :questions])))
                  2)))}
  [state playerId questionId]
  (assoc-in state
            [:players (keyword playerId) :questions]
            (map (fn [question]
                   (if (= (:id question) questionId)
                     (assoc question :focus true)
                     (assoc question :focus false)))
                 (get-in state [:players (keyword playerId) :questions]))))