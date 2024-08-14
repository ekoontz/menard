(ns menard.español.conjugate
  (:require [menard.español :as es]
            [menard.italiano :as it]
            [menard.nederlands :as nl]
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
  (let [persons [:1st :2nd-informal :2nd-formal :3rd]]
    {:singular (->> persons
                    (map (fn [person]
                           [person (-> (person person-map)
                                       (unify {:root canonical
                                               :infl inflection
                                               :subcat []
                                               :phrasal? true
                                               :agr {:number :sing}
                                               :comp {:phrasal? false}})
                                       log-and-generate
                                       morph)]))
                    (into {}))
     :plural   (->> persons
                    (map (fn [person]
                           [person (-> (person person-map)
                                       (unify {:root canonical
                                               :infl inflection
                                               :subcat []
                                               :phrasal? true
                                               :agr {:number :plur}
                                               :comp {:phrasal? false}})
                                       generate
                                       morph)]))
                    (into {}))}))

(defn generate-chart [canonical]
  {:canonical canonical
   :inflections [(merge {:name "Present"}
                        (verb canonical :present))
                 (merge {:name "Conditional"}
                        (verb canonical :conditional))
                 (merge {:name "Future"}
                        (verb canonical :future))
                 (merge {:name "Imperfect"}
                        (verb canonical :imperfect))
                 (merge {:name "Preterito"}
                        (verb canonical :preterito))]})
