(ns do-you-know-me.game
  (:require [clojure.test :refer :all]
            [do-you-know-me.QUESTIONS :refer [questions]]
            [do-you-know-me.utils :refer [in?]]))

(def game-states (atom {}))

(def emojis ["🐹" "🐶" "🐱" "🦊" "🐻" "🐯" "🦁" "🐸" "🐵" "🐥"])


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
                                         :questions   questions
                                         :players     []})
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
  (first (filter (fn [player]
                   (= (:id player) id)) (:players state))))


(defn get-players
  {:test (fn []
           (is (= (get-players (create-game "1234")) []))
           (is (= (count (get-players (-> (create-game "1234")
                                          (add-player "p1")
                                          (add-player "p2")))) 2)))}
  [state]
  (:players state))


(defn get-emojis-in-game
  {:test (fn []
           (is (= (count (-> (create-game "1234")
                             (add-player "p1")
                             (add-player "p2")
                             (get-emojis-in-game)))
                  2)))}
  [state]
  (map :emoji (get-players state)))


(defn pick-unique-emoji
  {:test (fn []
           (let [state (create-game "1234")]
             (is (= (:emoji (pick-unique-emoji (add-player state "p1")))
                    (:emoji (get-player state "p1"))))))}
  [state]
  (rand-nth (filter (fn [emoji]
                      (not (in? emoji (get-emojis-in-game state)))) emojis)))


(defn add-player
  {:test (fn []
           (is (= (count (:players (add-player (create-game "1234") "p1"))) 1))
           (is (= (:id (get-player (add-player (create-game "1234") "p1") "p1")) "p1")))}
  [state id]
  (-> state
      (update :players conj {:username  "unknown"
                             :id        id
                             :emoji     (pick-unique-emoji state)
                             :color     nil
                             :questions []
                             :ready     false})))
