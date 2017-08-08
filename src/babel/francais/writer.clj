(ns babel.francais.writer
  (:refer-clojure :exclude [get-in]))

(require '[babel.francais.grammar :refer [small medium]])
(require '[babel.francais.lexicon :refer [lexicon]])
(require '[babel.francais.morphology :refer [analyze exception-generator
                                             fo]])
(require '[babel.francais.pos :refer [intransitivize transitivize]])
(require '[babel.korma :refer [init-db]])
(require '[babel.writer :as writer :refer [process reload write-lexicon]])
(require '[clojure.tools.logging :as log])
(require '[dag_unify.core :refer (fail? get-in strip-refs unify)])

(defn rewrite-lexicon []
  (write-lexicon "fr" @lexicon))

(defn tout [ & [count lexeme no-older-than]]
  (let [use-map-fn pmap
        count (cond (nil? count) 10
                    (= "all" count) 10
                    true (Integer. count))
        deliver-lexicon (babel.francais.lexicon/deliver-lexicon)
        root-verbs 
        (zipmap
         (keys @lexicon)
         (use-map-fn
          (fn [lexeme-set]
            (filter (fn [lexeme]
                      (and
                       ;; uncomment to only generate for a desired verb, e.g. "se amuser"
                       ;;                           (= (get-in lexeme [:français :français]) "se amuser")
                       (= (get-in lexeme [:synsem :cat]) :verb)
                       (= (get-in lexeme [:synsem :infl]) :top)
                       (not (= :top (get-in lexeme [:synsem :sem :pred] :top)))))
                    lexeme-set))
          (vals @lexicon)))
        root-verb-array
        (reduce concat
                (use-map-fn (fn [key]
                              (get root-verbs key))
                            (sort (keys root-verbs))))]
    (init-db)
    (write-lexicon "fr" @lexicon)
    (log/info (str "done writing lexicon."))
    (log/info (str "generating examples with this many verbs:"
                   (.size root-verb-array)))
    (.size
     (->> root-verb-array
          (use-map-fn
           (fn [verb]
             (log/debug (str "verb: " (strip-refs verb)))
             (let [root-form (get-in verb [:français :français])
                   spec {:root {:français {:français root-form}}}]
               (log/info (str "generating with verb: '" root-form "'"))
               (writer/generate-from-spec
                (medium) (strip-refs spec)

                [ ;; tenses
;                 {:synsem {:sem {:tense :conditional}}}
                 
;                 {:synsem {:sem {:tense :future}}}
                 
;                 {:synsem {:sem {:tense :present
;                                 :aspect :simple}}}
                 
                 ;; imparfait
 ;                {:synsem {:sem {:aspect :progressive
;                                 :tense :past}}}
                 
                 ;; passé composé
                 {:synsem {:sem {:aspect :perfect
                                 :tense :present}}}
                 ]

                [{:gender :masc} {:gender :fem}]
                [:1st :2nd :3rd]
                [:sing :plur]
                count
                no-older-than))))))))


