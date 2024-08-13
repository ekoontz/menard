(ns menard.español.conjugate
  (:require [menard.español :refer [generate morph]]
            [dag_unify.core :as u :refer [unify]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

(def spec-map
  (->>
   [{:k :1st
     :a {:person :1st}}
    {:k :2nd-informal
     :a {:person :2nd
         :formal? false}}
    {:k :2nd-formal
     :a {:person :2nd
         :formal? true}}
    {:k :3rd
     :a {:person :3rd}}]

    (map (fn [{k :k a :a}]
           [k (unify
               {:agr {:number :sing}}
               {:agr a})]))

    (into {})))

(defn verb [canonical inflection]
  (let [singular-map [:1st :2nd-informal :2nd-formal :3rd]]
    {:canonical canonical
     :inflection inflection
     :singular (->> singular-map
                    (map (fn [person]
                           {person (-> (person spec-map)
                                       (merge {:root canonical
                                               :infl inflection
                                               :subcat []
                                               :phrasal? true
                                               :comp {:phrasal? false}})
                                       generate
                                       morph)})))}))


              

   

