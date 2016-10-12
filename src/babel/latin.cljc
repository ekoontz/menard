(ns babel.latin
  (:refer-clojure :exclude [get-in])
  (:require [babel.latin.morphology :as morph]
            [babel.lexiconfn :refer [default listify]]
            [clojure.java.io :refer [reader resource]]
            [clojure.repl :refer [doc]]
            [dag_unify.core :refer [get-in]]))

;; TODO: factor out Italian-specific parts and promote to babel.lexiconfn.
;; TODO: see if we can use Clojure transducers here. (http://clojure.org/reference/transducers)
(defn edn2lexicon [resource]
  (-> (read-string (slurp resource)) ;; read .edn file into a Clojure map.
      listify
      (default
       {:phrasal false})))

(def lexicon (edn2lexicon "src/babel/latin/lexicon.edn"))

(def model {:lexicon lexicon})

(defn pred2root [pred]
  (first (shuffle
          (filter (fn [root]
                    (let [vals (get lexicon root)]
                      (not (empty? (filter (fn [val]
                                             (= (get-in val [:synsem :sem :pred]) pred))
                                           vals)))))
                  (keys lexicon)))))

(defn generate [spec]
  (let [pred (get-in spec [:synsem :sem :pred])
        subject (get-in spec [:synsem :sem :subj :pred])
        verb (pred2root pred)]
    (morph/conjugate verb {:synsem {:sem {:subj {:pred subject}}}})))

               
