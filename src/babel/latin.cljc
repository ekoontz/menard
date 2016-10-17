(ns babel.latin
  (:refer-clojure :exclude [get-in])
  (:require [babel.latin.morphology :as morph]
            [babel.lexiconfn :refer [default listify map-function-on-map-vals
                                     verb-pred-defaults]]
            [babel.encyclopedia :as encyc]
            [clojure.java.io :refer [resource]]
            [clojure.repl :refer [doc]]
            [dag_unify.core :refer [fail? get-in unifyc]]))

(defn edn2lexicon [resource]
  (-> (read-string (slurp resource)) ;; read .edn file into a Clojure map.
      listify
      (map-function-on-map-vals
       (fn [lexicon-string lexemes]
         (map (fn [lexeme]
                (merge lexeme
                       {:root lexicon-string}))
              lexemes)))
      (default
       {:phrasal false})))

(def lexicon
  (-> (edn2lexicon (resource "babel/latin/lexicon.edn"))
      (verb-pred-defaults encyc/verb-pred-defaults)))

(defn parse [surface]
  [{:parses (morph/analyze surface lexicon)}])

(defn fo [structure]
  (morph/conjugate (get-in structure [:root]) structure))

(declare generate)

(def tenses
  [{:tense :present}
   {:tense :past
    :aspect :progressive}
   {:tense :future}])

(def subjects [:I :tu :lui :lei :noi :voi :loro])

(def preds
  (vec
   (set
    (map #(get-in % [:synsem :sem :pred]) 
         (filter #(= :verb (get-in % [:synsem :cat]))
                 (flatten (vals lexicon)))))))
(def roots
  (vec
   (set
    (map #(get-in % [:root])
         (filter #(= :verb (get-in % [:synsem :cat]))
                 (flatten (vals lexicon)))))))

(defn generate [spec]
  (let [expr
        (first (shuffle
                (filter #(not (fail? %))
                        (map (fn [val]
                               (unifyc spec val))
                             (flatten (vals lexicon))))))]
    (if expr
      (conj {:surface (fo expr)}
            expr))))

(defn get-spec [base-spec]
  "return a spec that is more specific than base-spec, specific enough to conjugate."
  (unifyc
   base-spec
   (or (and (get-in base-spec [:synsem :sem :tense])
            base-spec)
       {:synsem {:sem (first (shuffle tenses))}})
   (or (and (get-in base-spec [:synsem :subj :pred])
            base-spec)
       {:synsem {:sem {:subj {:pred (first (shuffle subjects))}}}})
   (or (and (get-in base-spec [:root])
            base-spec)
       {:root (first (shuffle roots))})))

(def model {:lexicon lexicon
            :fo fo
            :generate-fn generate})
