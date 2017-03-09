(ns babel.italiano.writer
  (:refer-clojure :exclude [get-in]))

(require '[babel.generate :as generate])
(require '[babel.italiano.grammar :as grammar :refer [create-model-for-spec]])
(require '[babel.italiano.morphology :as morph])
(require '[babel.korma :refer [init-db]])
(require '[babel.writer :as writer :refer [process write-lexicon]])
(require '[clojure.string :refer [join]])
(require '[clojure.tools.logging :as log])
(require '[dag_unify.core :refer (fail? get-in strip-refs unify)])

(def small (grammar/small))  ;; TODO use medium consistently

(defn expression [spec]
  (generate/generate small spec))  ;; TODO use medium consistently

(defn fo [spec]
  (morph/fo spec))

(defn rewrite-lexicon []
  (let [lexicon (:lexicon small)] ;; TODO use medium consistently
    (write-lexicon "it" lexicon)))

(declare generate-one-verb)

(defn tutti [ & [count lexeme no-older-than]]
  (let [count (cond (nil? count) 10
                    (= "all" count) 10
                    true (Integer. count))
        ;; subset of the lexicon: only verbs which are infinitives and that can be roots:
        ;; (i.e. those that have a specific (non- :top) value for [:synsem :sem :pred])
        lexicon (:lexicon small) ;; TODO use medium consistently
        lexemes
        (cond (nil? lexeme) (vals lexicon)
              (= "all" lexeme) (vals lexicon)
              true (list (get lexicon lexeme)))
        root-verbs 
        (zipmap
         (keys lexicon)
         (map (fn [lexeme-set]
                (filter (fn [lexeme]
                          (and
                           (= (get-in lexeme [:synsem :cat]) :verb)
                           (= (get-in lexeme [:synsem :infl]) :top)
                           (= (get-in lexeme [:synsem :aux] false) false)
                           (not (= :top (get-in lexeme [:synsem :sem :pred] :top)))))
                        lexeme-set))
              lexemes))

        tutti
        (reduce concat
                (map (fn [key]
                       (get root-verbs key))
                     (sort (keys root-verbs))))]

    ;; TODO: lexicon-writing should be moved out of (defn tutti []).
    (init-db)
    (write-lexicon "it" lexicon)
    (log/info (str "done writing lexicon."))
    (log/info (str "generating examples with this many verbs:"
                   (.size tutti)))
    (.size (pmap (fn [verb]
                   (log/debug (str "tutti: verb: " (strip-refs verb)))
                   (let [root-form (get-in verb [:italiano :italiano])]
                     (generate-one-verb (unify
                                         {:synsem {:subcat '()}}
                                         {:synsem {:sem
                                                   (get-in verb [:synsem :sem] :top)}}
                                         {:root {:italiano {:italiano root-form}}})
                                        count no-older-than)))
                 tutti))))

(defn generate-one-verb [spec & [count no-older-than]]
  (log/debug (str "generate-one-verb with spec:" spec "; count=" count))
  (writer/generate-from-spec
   (let [model (create-model-for-spec spec)]
     (log/debug (str "created custom model from spec: " spec "; new model: " (keys model)))
     model)
   (strip-refs spec)

   (cond
     (= (get-in spec [:root :italiano :italiano])
        "chiamarsi")
     ;; for this verb, generate only present
     [{:synsem {:sem {:tense :present
                      :aspect :simple}}}]
     true [;; conditional
           {:synsem {:sem {:tense :conditional}}}

           ;; future
           {:synsem {:sem {:tense :future}}}

           ;; simple present
           {:synsem {:sem {:tense :present
                           :aspect :simple}}}

           ;; imperfetto
           {:synsem {:sem {:aspect :progressive
                           :tense :past}}}

           ;; passato prossimo
           {:synsem {:sem {:aspect :perfect
                           :tense :present}}}

           ;; trapassato
           {:synsem {:sem {:aspect :pluperfect
                           :tense :past}}}

           ;; present progressive
           {:synsem {:sem {:aspect :progressive
                           :tense :present}}}
           ])

   [{:gender :masc}
    {:gender :fem}]

   (cond (= (get-in spec [:root :italiano :italiano])
            "funzionare")
         [:3rd]

         (and
          (= (get-in spec [:synsem :sem :pred]) :exist)
          (= (get-in spec [:root :italiano :italiano])
             "essere"))
         [:3rd]
         
         true
         [:1st :2nd :3rd])

   (cond (= (get-in spec [:root :italiano :italiano])
            "chiamarsi")
         [:sing]
         true
         [:sing :plur])
   
   count
   no-older-than))

