(ns menard.italiano.conjugate
  (:require [menard.italiano :as it]
            [dag_unify.core :as u :refer [unify]]
            [dag_unify.serialization :refer [serialize]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

(def person-map
  {:1st {:agr {:person :1st}},
   :2nd-informal {:agr {:person :2nd, :formal? false}},
   :2nd-formal {:agr {:person :2nd, :formal? true}},
   :3rd {:agr {:person :3rd}}})

(defn log-and-generate [spec]
  (log/info (str "generating with spec: " spec))
  (it/generate spec))

(defn add-reflexive [spec canonical]
  (log/info (str "preprocess-spec: spec: " spec "; canonical: " canonical))
  (cond
    (re-find #"[aei]rse$" canonical)
    (let [with-reflexive-true (unify spec {:reflexive? true})]
      (if (not (= :fail with-reflexive-true))
        with-reflexive-true
        spec))
    (re-find #"[aei]r$" canonical)
    (let [with-reflexive-false (unify spec {:reflexive? false})]
      (if (not (= :fail with-reflexive-false))
        with-reflexive-false
        spec))
    :else spec))

(defn add-rules [spec inflection-variant canonical]
  (let [retval
        (cond
          (and
           (= inflection-variant :preterito-perfecto)
           (re-find #"[aei]rse$" canonical))
          (unify spec
                 {:rule "s-aux"
             :head {:rule "vp-aux-reflexive-3"}})
          (= inflection-variant :preterito-perfecto)
          (unify spec
                 {:rule "s-aux"
                  :head {:rule "vp-aux-non-reflexive"}})
          :else
          spec)]
    (log/debug (str "add-rules returning spec: " retval))
    retval))

(defn verb [canonical inflection async?]
  (let [generate-fn (fn [spec]
                      (if async?
                        (-> spec serialize) ;; in this case, generation is deferred to an asynchronous call,
                        ;; and we serialize the spec so that it can be passed in an encoded form
                        ;; to that async call.

                        (-> spec (it/generate it/curated-verbs) it/morph))) ;; in this case, generate now.
        basic-spec {:cat :verb
                    :root canonical
                    :comp {:pronoun? true
                           :phrasal? false}
                    :subcat []}
        inflection-spec (->> menard.italiano.tenses/finite-tenses
                             (filter #(= inflection (u/get-in % [:variant])))
                             first)
        spec (-> (unify basic-spec inflection-spec)
                 (add-reflexive canonical)
                 (add-rules inflection canonical))]
    (log/debug (str "generating with basic-spec: " basic-spec))
    (log/debug (str "generating with inflection-spec: " inflection-spec))
    (log/debug (str "generating with spec: " spec))
    (let [persons [:1st :2nd-informal :2nd-formal :3rd]]
      {:singular (->> persons
                      (map (fn [person]
                             [person (-> (person person-map)
                                         (unify spec)                                         
                                         (unify {:agr {:number :sing}})
                                         generate-fn)]))
                      (into {}))
       :plural   (->> persons
                      (map (fn [person]
                             [person (-> (person person-map)
                                         (unify spec)
                                         (unify {:agr {:number :plur}})
                                         generate-fn)]))
                      (into {}))})))

(defn generate-chart [canonical async?]
  {:canonical canonical
   :moods [{:name "Modo indicativo" :css-class "indicativo"
            :inflections [(merge {:name "Presente"}
                                 (verb canonical :present-simple async?))
                          (merge {:name "Pretérito imperfecto"}
                                 (verb canonical :imperfect async?))
                          (merge {:name "Pretérito perfecto"}
                                 (verb canonical :preterito async?))
                          (merge {:name "Pretérito perfecto compuesto"}
                                 (verb canonical :preterito-perfecto async?))
                          (merge {:name "Futuro"}
                                 (verb canonical :future async?))]}
           {:name "Modo condicional" :css-class "condicional"
            :inflections [(merge {:name "Condicional simple"}
                                 (verb canonical :conditional async?))]}]})

          
