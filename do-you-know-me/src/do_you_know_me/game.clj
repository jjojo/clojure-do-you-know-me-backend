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
  (assoc-in state [:players (keyword id)] {:username  "unknown"
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
