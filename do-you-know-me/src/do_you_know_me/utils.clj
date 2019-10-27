(ns do-you-know-me.utils)

(defn in?
  "true if coll contains elm"
  [coll elm]
  (some #(= elm %) coll))
