(ns menard.english.complete
  (:require [dag_unify.core :as u]
            [clojure.tools.logging :as log]
            [menard.english.compile :refer [compile-lexicon]]
            [menard.model :refer [create]]))

(def create-model? true)

#?(:clj
   (if create-model?
     (def model
       (ref (create "english/models/complete"
                    compile-lexicon)))))

