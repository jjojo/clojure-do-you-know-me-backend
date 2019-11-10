(ns do-you-know-me.core
  (:require [org.httpkit.server :refer [run-server]]
            [clojure.test :refer :all]
            [do-you-know-me.handler :refer [handler]]))


(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!")
  (run-server handler {:port      9090
                       :insecure? true
                       }))
