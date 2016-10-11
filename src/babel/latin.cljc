(ns babel.latin
  (:require [babel.latin.morphology :as morph]
            [babel.lexiconfn :refer [default listify]]
            [clojure.java.io :refer [reader resource]]
            [clojure.repl :refer [doc]]))

;; TODO: factor out Italian-specific parts and promote to babel.lexiconfn.
;; TODO: see if we can use Clojure transducers here. (http://clojure.org/reference/transducers)
(defn edn2lexicon [resource]
  (-> (read-string (slurp resource)) ;; read .edn file into a Clojure map.
      listify
      (default
       {:phrasal false})))

(def lexicon (edn2lexicon "src/babel/latin/lexicon.edn"))

(def model {:lexicon lexicon})
