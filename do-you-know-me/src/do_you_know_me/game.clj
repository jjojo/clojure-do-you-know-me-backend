(ns do-you-know-me.game
  (:require [clojure.test :refer :all]))

(defn create-game
  {:test (fn []
           (is (= (:title (create-game)) "Do you know me? 🤔")))}
  []
  {:title "Do you know me? 🤔"})
