(ns babel.latin
  (:refer-clojure :exclude [get-in])
  (:require [babel.latin.morphology :as morph]
            [babel.lexiconfn :refer [default listify map-function-on-map-vals
                                     verb-pred-defaults]]
            [babel.encyclopedia :as encyc]
            [clojure.java.io :refer [resource]]
            [clojure.repl :refer [doc]]
            [dag_unify.core :refer [fail? get-in strip-refs unifyc]]))

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


      (default ;; intransitive verbs' :obj is :unspec.
       {:synsem {:cat :verb
                 :subcat {:1 {:top :top}
                          :2 '()}
                 :sem {:obj :unspec}}})

      (verb-pred-defaults encyc/verb-pred-defaults)))

(defn parse [surface]
  [{:parses (morph/analyze surface lexicon)}])

;; can't decide which to use "fo" or "morph", if either.
(defn fo [structure]
  (morph/conjugate (get-in structure [:root]) structure))

(defn morph [structure]
  (fo structure))
    
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
            :morph fo
            :generate-fn generate})
(defn intersection [curriculum model]
  ;; TODO implement this stub
  model)

(defn choose-spec [curriculum model]
  "choose a random spec based on the given curriculum and model"
  ;; for now, stubbed out: imagine a curriculum narrowly based on a single verb and
  ;; the imperfect tense.
  (get-spec
   {:root "ardēre"
    :synsem {:sem {:tense :past
                   :aspect :progressive}}}))
  
(def curriculum
  {:nouns ["lui" "lei"]
   :verbs :all
   :tenses :all})

(def custom-model
  (intersection
   curriculum
   model))

(def source-model (babel.english.grammar/small))

(defn read-one []
  (let [spec (choose-spec curriculum model)
        target-expression (generate spec)
        semantics-of-target-expression (get-in target-expression [:synsem :sem])
        question-to-pose-to-user
        (babel.english/morph (babel.english/generate {:synsem {:sem semantics-of-target-expression}}
                                                     :model source-model)
                             :show-notes false) ;; TODO: use {:from-language :la}
        parses (babel.english/parse question-to-pose-to-user)
        semantics-of-source-expression
        (set (map #(get-in % [:synsem :sem])
                  parses))]
    {:source question-to-pose-to-user
     :subj (get-in (first semantics-of-source-expression) [:subj :pred])
     :targets (vec (set (map #(morph (generate {:synsem {:sem %}}))
                             semantics-of-source-expression)))}))


