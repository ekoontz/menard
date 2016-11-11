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
                      (do (log/info (str "starting to load english model.."))
                          (let [model (babel.english.grammar/small)]
                            (log/info (str "finished loading english model."))
                            (conj model
                                  {:generate-fn (fn [spec]
                                                  (en/generate spec :model model))}))))))
     :es (fn []
           (if (realized? es)
             es
             (deliver es
                      (do (log/info (str "starting to load Español model.."))
                          (let [model (babel.espanol.grammar/small)]
                            (log/info (str "finished loading Espanol model."))
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
                      (do (log/info (str "starting to load French model.."))
                          (let [model (babel.francais.grammar/medium)]
                            (log/info (str "finished loading French model."))
                            (conj model
                                  {:generate-fn (fn [spec]
                                                  (fr/generate spec :model model))}))))))
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
     (map (fn [root-verb]
            (do (println (str "" root-verb))
                (let [root-verb-spec (get root-verb-specs root-verb)]
                  (doall (map (fn [tense]
                                (println (str " " tense))
                                (doall (map (fn [number]
                                              (let [number-spec {:comp {:synsem {:agr {:number number}}}}]
                                                (doall (map (fn [person]
                                                              (let [person-spec {:comp {:synsem {:agr {:person person}}}}]
                                                                (println (str "  " (generate language (unify root-verb-spec
                                                                                                             (get tenses tense)
                                                                                                             number-spec
                                                                                                             person-spec))))))
                                                            persons))))
                                            numbers)))
                              (sort (keys tenses)))))
                (println)))
          (sort (keys root-verb-specs))))))

(defn generate [language spec]
  (let [model @((-> models language))
        generate-fn (:generate-fn model)
        morph-fn (:morph model)]
    (log/debug (str "generating with spec: " spec))
    (let [result (generate-fn spec)]
      (when (nil? result)
        (let [err (str "could not generate an expression for spec: " spec " in language:" language)]
          (log/error err) 
          (throw (Exception. err))))
      (morph-fn result))))







