(ns babel.latin
  (:refer-clojure :exclude [get-in])
  (:require
   [babel.index :refer [build-lex-sch-index create-indices lookup-spec]]
   [babel.latin.morphology :as morph]
   [babel.lexiconfn :refer [default listify map-function-on-map-vals
                            verb-pred-defaults]]
   [babel.encyclopedia :as encyc]
   [clojure.java.io :refer [resource]]
   [clojure.repl :refer [doc]]
   [clojure.tools.logging :as log]
   [dag_unify.core :refer [fail? get-in strip-refs unify]]))

(def index-lexicon-on-paths
  [[:synsem :cat]
   [:synsem :aux]
   [:synsem :sem :pred]])

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

(defn parse [surface model]
  (let [lexicon (:lexicon model)]
    [{:parses (morph/analyze surface lexicon)}]))

;; can't decide which to use "fo" or "morph", if either.
(defn fo [structure]
  (morph/conjugate (get-in structure [:root]) structure))

(defn morph [structure]
  (fo structure))

(declare generate)

(defn model []
  (let [lexicon
        (-> (edn2lexicon (resource "babel/latin/lexicon.edn"))
          
            (default ;; intransitive verbs' :obj is :unspec.
             {:synsem {:cat :verb
                       :subcat {:1 {:top :top}
                                :2 '()}
                       :sem {:obj :unspec}}})
            
            (verb-pred-defaults encyc/verb-pred-defaults))
        indices (create-indices lexicon index-lexicon-on-paths)]
    {:lexicon lexicon
     :index-fn (fn [spec] (lookup-spec spec indices index-lexicon-on-paths))
     :morph morph
     :generate-fn generate}))

(def tenses
  [{:tense :present}
   {:tense :past
    :aspect :progressive}
   {:tense :future}])

(def verb-agreement
  [{:agr {:person :1st
          :number :sing}}
   {:agr {:person :2nd
          :number :sing}}])

(defn preds [lexicon]
  (vec
   (set
    (map #(get-in % [:synsem :sem :pred]) 
         (filter #(= :verb (get-in % [:synsem :cat]))
                 (flatten (vals lexicon)))))))

(defn roots [lexicon]
  (vec
   (set
    (map #(get-in % [:root])
         (filter #(= :verb (get-in % [:synsem :cat]))
                 (flatten (vals lexicon)))))))

(defn generate [spec model]
  (let [lexicon (:lexicon model)
        expr
        (first (shuffle
                (filter #(not (fail? %))
                        (map (fn [val]
                               (unify spec val))
                             (flatten (vals lexicon))))))]
    (if expr
      (conj {:surface (fo expr)}
            expr))))

(defn get-spec
  "return a spec that is more specific than base-spec, specific enough to conjugate."
  [base-spec model]
  (log/debug (str "get-spec:base-spec:" base-spec))
  (let [retval
        (unifyc
         base-spec

         ;; Read 'em their rights:
         ;; 
         ;; 1. You have the right to a tense. If you do not have one,
         ;; get-spec will appoint one for you.
         (or (and (get-in base-spec [:synsem :sem :tense])
                  base-spec)
             {:synsem {:sem (first (shuffle tenses))}})
         
         ;; 2. You have the right to verb agreement. If you do not have any,
         ;; get-spec will appoint some for you.
         (or (and (get-in base-spec [:synsem :agr :number])
                  base-spec)
             {:synsem (first (shuffle verb-agreement))})
         
         ;; 3. You have the right to a root. If you do not have one,
         ;; get-spec will appoint one for you.
         (or (and (get-in base-spec [:root])
                  base-spec)
             {:root (first (shuffle (roots (:lexicon model))))}))]
    (log/debug (str "get-spec: returning:" (dissoc (strip-refs retval)
                                                   :dag_unify.core/serialized)))
    retval))

(defn intersection [spec curriculum model]
  (cond false ;; TODO implement this stub and set to true
        (unify spec
               ;; e.g. support curriculum was
               ;; drilling on imperfect, we would have:
               {:synsem {:sem {:tense :past
                               :aspect :imperfect}}})
        true
        spec))

(defn choose-spec [spec curriculum model]
  "choose a random spec based on the given curriculum and model"
  (let [spec-from-curriculum (intersection spec curriculum model)]
    (get-spec spec-from-curriculum model)))
  
(def curriculum
  {:nouns ["lui" "lei"]
   :verbs :all
   :tenses :all})

(defn custom-model [curriculum model]
  "create a language model especially for a given curriculum that is a subset of the given model"
  (intersection :top
                curriculum
                model))

(defn read-one [spec target-model source-model]
  (let [spec (choose-spec spec curriculum target-model)
        debug (log/debug (str "read-one: chosen spec: "
                              (dissoc (strip-refs spec)
                                      :dag_unify.core/serialized)))
        target-expression (generate spec target-model)
        debug (log/debug (str "read-one: target-expression:"
                              (dissoc (strip-refs target-expression)
                                      :dag_unify.core/serialized)))
        semantics-of-target-expression (get-in target-expression [:synsem :sem])
        debug (log/debug (str "read-one: semantics-of-target-expression:" semantics-of-target-expression))
        source-specification {:synsem {:sem semantics-of-target-expression}
                              :comp {:synsem {:pronoun true
                                              :agr (get-in spec [:synsem :agr])}}}
        debug (log/debug (str "read-one: source-specification:" source-specification))
        question-to-pose-to-user
        (babel.english/morph (babel.english/generate source-specification
                                                     :model source-model
                                                     :truncate-children false)
                             :show-notes false) ;; TODO: use {:from-language :la}
        parses (babel.english/parse question-to-pose-to-user false) ;; false: don't truncate
        source-expressions parses
        target-specs
        (map (fn [parse]
               {:synsem {:sem (get-in parse [:synsem :sem])
                         :agr (get-in parse [:comp :synsem :agr] :top)}})
             source-expressions)]
    (log/debug (str "parses comps:"
                    (clojure.string/join "," (map #(strip-refs (get-in % [:comp]))
                                                  parses))))
    (log/debug (str "target-specs:"
                    (clojure.string/join "," (strip-refs target-specs))))
                    
    {:source question-to-pose-to-user
     :semantics (strip-refs semantics-of-target-expression)
     :targets (vec (set (map #(morph (generate {:synsem {:sem (get-in % [:synsem :sem])
                                                         :agr (get-in % [:comp :synsem :agr] :top)}}
                                               target-model))
                             source-expressions)))}))


