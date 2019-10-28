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
  (rand-nth (filter (fn [item]
                      (not (some #(= item %) (get-coll-players-key state key))))
                    coll)))


(defn add-player
  {:test (fn []
           (is (= (count (:players (add-player (create-game "1234") "p1"))) 1))
           (is (= (:id (get-player (add-player (create-game "1234") "p1") "p1")) "p1")))}
  [state id]
  (-> state
      (update :players conj {:username  "unknown"
                             :id        id
                             :emoji     (pick-unique-key state emojis :emoji)
                             :color     (pick-unique-key state colors :color)
                             :questions []
                             :ready     false})))
