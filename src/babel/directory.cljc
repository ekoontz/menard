(ns babel.directory
  (:refer-clojure :exclude [get-in])
  (:require
   [babel.english :as en]
   [babel.espanol :as es]
   [babel.francais :as fr]
   [babel.latin :as la]
   [babel.italiano :as it]
   [clojure.tools.logging :as log]
   [dag_unify.core :refer [get-in unify]]))

(declare generate-pair)

;; TODO: understand promises, futures, (deliver), etc, better: this code is susceptible to
;; race conditions when used with pmap (e.g. with generate)


(def models2
;; make finer-grained distinction between things that are slow to load (lexicon and indices)
  ;; vs fast to load: e.g. simple constant values and simple maps of the same.
  (let [en (promise)
        es (promise)
        fr (promise)
        la (promise)
        it (promise)]
    {:en (fn []
           (if (realized? en)
             en
             (deliver en
                      {:morph en/morph
                       :semantic-correspondence {:it [[:obj :null]
                                                      [:obj :number]
                                                      [:shared-with-obj]
                                                      [:subj :null]
                                                      [:subj :number]]}})))
     :es (fn []
           (if (realized? es)
             es
             (deliver es
                      (let [model {}]
                        (conj model
                              {:tenses babel.espanol.grammar/tenses})))))
     :fr (fn []
           (if (realized? fr)
             fr
             (deliver fr
                      (let [model {}]
                        (conj model
                              {:tenses babel.francais.grammar/tenses})))))
     :la (fn []
           (if (realized? la)
             la
             (deliver la
                      (let [model {}]
                        (conj model)))))
     :it (fn []
           (if (realized? it)
             it
             (deliver it
                      (let [model {}]
                        (conj model
                              {:tenses babel.italiano.grammar/tenses})))))}))

(def models
  (let [en (promise)
        es (promise)
        fr (promise)
        la (promise)
        it (promise)]
    {:en (fn []
           (if (realized? en)
             en
             (deliver en 
                      (do (log/debug (str "starting to load english model.."))
                          (let [model (babel.english.grammar/medium)]
                            (log/info (str "finished loading english model: "
                                           (:name model)))
                            model)))))
     :es (fn []
           (if (realized? es)
             es
             (deliver es
                      (do (log/debug (str "starting to load Español model.."))
                          (let [model (babel.espanol.grammar/small)]
                            (log/debug (str "finished loading Espanol model."))
                            (conj model
                                  {:generate-fn (fn [spec]
                                                  (es/generate spec :model model))
                                   :tenses babel.espanol.grammar/tenses
                                   :root-verb-specs
                                   (into {}
                                         (map (fn [root]
                                                [root {:root {:espanol {:espanol root}}}])
                                              (sort
                                               (remove nil? (map (fn [val]
                                                                   (dag_unify.core/get-in val [:espanol :espanol]))
                                                                 (filter (fn [v]
                                                                           (and (= :top (dag_unify.core/get-in v [:synsem :infl]))
                                                                                (= :verb (dag_unify.core/get-in v [:synsem :cat]))))
                                                                         (flatten (vals (:lexicon model)))))))))}))))))
  
     :fr (fn []
           (if (realized? fr)
             fr
             (deliver fr
                      (do (log/debug (str "starting to load French model.."))
                          (let [model (babel.francais.grammar/medium)]
                            (log/debug (str "finished loading French model."))
                            (conj model
                                  {:generate-fn (fn [spec]
                                                  (fr/generate spec :model model))
                                   :tenses babel.francais.grammar/tenses
                                   :root-verb-specs
                                   (into {}
                                         (map (fn [root]
                                                [root {:root {:français {:français root}}}])
                                              (sort
                                               (remove nil? (map (fn [val]
                                                                   (dag_unify.core/get-in val [:français :français]))
                                                                 (filter (fn [v]
                                                                           (and (= :top (dag_unify.core/get-in v [:synsem :infl]))
                                                                                (= :verb (dag_unify.core/get-in v [:synsem :cat]))))
                                                                         (flatten (vals (:lexicon model)))))))))}))))))
     :la (fn []
           (if (realized? la)
             la
             (deliver la
                      (do (log/info (str "starting to load latin model.."))
                          (let [model (la/model)]
                            (log/info (str "finished loading latin model."))
                            (conj model
                                  {:generate-fn (fn [spec]
                                                  (la/generate spec model))}))))))
     :it (fn []
           (if (realized? it)
             it
             (deliver it
                      (let [model (babel.italiano.grammar/small)]
                        (conj model
                              {:generate-fn (fn [spec]
                                              (it/generate spec :model model))
                               :tenses babel.italiano.grammar/tenses
                               :root-verb-specs
                               (into {}
                                     (map (fn [root]
                                            [root {:root {:italiano {:italiano root}}}])
                                          (sort
                                           (remove nil? (map (fn [val]
                                                               (dag_unify.core/get-in val [:italiano :italiano]))
                                                             (filter (fn [v]
                                                                       (and (= :top (dag_unify.core/get-in v [:synsem :infl]))
                                                                            (= :verb (dag_unify.core/get-in v [:synsem :cat]))))
                                                                     (flatten (vals (:lexicon model)))))))))})))))}))
  
(declare generate)

(defn generate-all [language]
  (let [language (if (string? language)
                   (keyword language)
                   language)
        model @((-> models language))
        root-verb-specs
        (:root-verb-specs model)
        tenses (:tenses model)
        persons [:1st :2nd :3rd]
        numbers [:sing :plur]]
    (doall
     (->>
      (sort (keys root-verb-specs))
      (map (fn [root-verb]
             (do (println (str "" root-verb))
                 (let [root-verb-spec (get root-verb-specs root-verb)]
                   (doall
                    (->>
                     (sort (keys tenses))
                     (map (fn [tense]
                            (println (str " " tense))
                            (doall
                             (->>
                              numbers
                              (map
                               (fn [number]
                                 (let [number-spec {:comp {:synsem {:agr {:number number}}}}]
                                   (doall
                                    (->>
                                     persons
                                     (map (fn [person]
                                            (let [person-spec {:comp {:synsem {:agr {:person person}}}}]
                                              (println
                                               (str "  "
                                                    (let [pair
                                                          (generate-pair
                                                           language (unify root-verb-spec
                                                                           (get tenses tense)
                                                                           number-spec
                                                                           person-spec))]
                                                      (str "\"" (:source pair) "\""
                                                           " -> "
                                                           "\""  (:target pair) "\"")))))))))))))))))))
                 (println))))))))

(defn generate-pair [language spec]
  (let [model @((-> models language))
        generate (:generate-fn model)
        morph-fn (:morph model)

        source-model @((-> models :en))
        source-generate (:generate-fn source-model)
        source-morph (:morph source-model)]
        
    (log/debug (str "generating with spec: " spec))
    (let [expression (try (generate spec)
                          (catch Exception e
                            (let [err (str "generate-pair: exception hit when trying to generate "
                                           "expression for spec: " spec " in target language: " language)]
                              (log/error err))))]
      (when (nil? expression)
        (let [err (str "generate-pair expression was nil for spec: " spec " in language:" language)]
          (log/error err) ))
      {:target
       (morph-fn expression)
       :source
       (source-morph (source-generate {:synsem {:sem (get-in expression [:synsem :sem])}}))})))
