(ns babel.latin
  (:refer-clojure :exclude [get-in])
  (:require
   [babel.latin.morphology :as morph]
   [babel.lexiconfn :refer [default listify map-function-on-map-vals
                            verb-pred-defaults]]
   [babel.encyclopedia :as encyc]
   [clojure.java.io :refer [resource]]
   [clojure.repl :refer [doc]]
   [clojure.tools.logging :as log]
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
  (log/debug (str "get-spec:base-spec:" base-spec ";subj: " (get-in base-spec [:synsem :subj :pred])))
  (unifyc
   base-spec

   ;; Read 'em their rights:
   ;; 
   ;; 1. You have the right to a tense. if you do not have one,
   ;; get-spec will appoint one for you.
   (or (and (get-in base-spec [:synsem :sem :tense])
            base-spec)
       {:synsem {:sem (first (shuffle tenses))}})

   ;; 2. You have the right to a subject. if you do not have one,
   ;; get-spec will appoint one for you.
   (or (and (get-in base-spec [:synsem :sem :subj :pred])
            base-spec)
       {:synsem {:sem {:subj {:pred (first (shuffle subjects))}}}})
   (or (and (get-in base-spec [:root])
            base-spec)
       {:root (first (shuffle roots))})))

(def model {:lexicon lexicon
            :morph fo
            :generate-fn generate})

(defn intersection [spec curriculum model]
  (cond false ;; TODO implement this stub and set to true
        (unifyc spec
                ;; e.g. support curriculum was
                ;; drilling on imperfect, we would have:
                {:synsem {:sem {:tense :past
                               :aspect :imperfect}}})
        true
        spec))

(defn choose-spec [spec curriculum model]
  "choose a random spec based on the given curriculum and model"
  ;; for now, stubbed out: imagine a curriculum narrowly based on a single verb and
  ;; the imperfect tense.
  (let [spec-from-curriculum (intersection spec curriculum model)]
    (get-spec spec-from-curriculum)))
  
(def curriculum
  {:nouns ["lui" "lei"]
   :verbs :all
   :tenses :all})

(defn custom-model [curriculum model]
  "create a language model especially for a given curriculum that is a subset of the given model"
  (intersection :top
                curriculum
                model))

(def source-model (babel.english.grammar/small))

(defn read-one [spec]
  (let [spec (choose-spec spec curriculum model)
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
     :semantics (strip-refs semantics-of-target-expression)
     :targets (vec (set (map #(morph (generate {:synsem {:sem %}}))
                             semantics-of-source-expression)))}))


