(ns babylon.generatenew
  (:require
   [babylon.generate :as g :refer [add add-lexeme add-rule foldup headness?
                                   numeric-frontier remove-trailing-comps
                                   truncate-at update-syntax-tree]]
   [dag_unify.serialization :as s]
   [dag_unify.core :as u]
   [clojure.tools.logging :as log]))

(defn generate-all [trees]
  (if (not (empty? trees))
    (let [tree (first trees)]
      (if (u/get-in tree [:babylon.generate/done?])
        (cons tree
              (generate-all (rest trees)))
        (lazy-cat
         (generate-all (add tree))
         (generate-all (rest trees)))))))

(defn generate [spec]
   (-> [spec] generate-all first))
