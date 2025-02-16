(ns menard.español.conjugate
  (:require [menard.español :as es]
            [dag_unify.core :as u :refer [unify]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

(def person-map
  {:1st {:agr {:person :1st}},
   :2nd-informal {:agr {:person :2nd, :formal? false}},
   :2nd-formal {:agr {:person :2nd, :formal? true}},
   :3rd {:agr {:person :3rd}}})

(defn log-and-generate [spec]
  (log/debug (str "generating with spec: " spec))
  (es/generate spec))

(defn verb [canonical inflection]
  (let [basic-spec {:cat :verb
                    :root canonical
                    :subcat []}
        inflection-spec (->> menard.español.tenses/finite-tenses
                             (filter #(= inflection (u/get-in % [:variant])))
                             first)
        spec (unify basic-spec inflection-spec)]
    (log/debug (str "generating with basic-spec: " basic-spec))
    (log/debug (str "generating with inflection-spec: " inflection-spec))
    (log/info (str "generating with spec: " spec))
    (let [persons [:1st :2nd-informal :2nd-formal :3rd]]
      {:singular (->> persons
                      (map (fn [person]
                             [person (-> (person person-map)
                                         (unify spec)
                                         (unify {:agr {:number :sing}})
                                         es/generate
                                         es/morph)]))
                      (into {}))
       :plural   (->> persons
                      (map (fn [person]
                             [person (-> (person person-map)
                                         (unify basic-spec)
                                         (unify inflection-spec)
                                         (unify {:agr {:number :plur}})
                                         es/generate
                                         es/morph)]))
                      (into {}))})))

(defn generate-chart [canonical]
  {:canonical canonical
   :inflections [(merge {:name "Present"}
                        (verb canonical :present-simple))
                 (merge {:name "Conditional"}
                        (verb canonical :conditional))
                 (merge {:name "Future"}
                        (verb canonical :future))
                 (merge {:name "Imperfect"}
                        (verb canonical :imperfect))
                 (merge {:name "Preterito"}
                        (verb canonical :preterito))
                 (merge {:name "Preterito Perfecto"}
                        (verb canonical :preterito-perfecto))
                 ]})
