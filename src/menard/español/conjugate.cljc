(ns menard.español.conjugate
  (:require [menard.español :refer [generate morph]]
            [dag_unify.core :as u :refer [unify]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

(def person-map
  {:1st {:agr {:person :1st}},
   :2nd-informal {:agr {:person :2nd, :formal? false}},
   :2nd-formal {:agr {:person :2nd, :formal? true}},
   :3rd {:agr {:person :3rd}}})

(defn verb [canonical inflection]
  (let [persons [:1st :2nd-informal :2nd-formal :3rd]]
    {:canonical canonical
     :inflection inflection
     :singular (->> persons
                    (map (fn [person]
                           {person (-> (person person-map)
                                       (unify {:root canonical
                                               :infl inflection
                                               :subcat []
                                               :phrasal? true
                                               :agr {:number :sing}
                                               :comp {:phrasal? false}})
                                       generate
                                       morph)})))
     :plural (->> persons
                  (map (fn [person]
                         {person (-> (person person-map)
                                     (unify {:root canonical
                                             :infl inflection
                                             :subcat []
                                             :phrasal? true
                                             :agr {:number :plur}
                                             :comp {:phrasal? false}})
                                     generate
                                     morph)})))}))

(defn generate-chart [canonical]
  {:present (verb canonical :present)
   :future (verb canonical :conditional)
   :conditional (verb canonical :conditional)
   :imperfect (verb canonical :imperfect)
   :preterito (verb canonical :preterito)})


              

   

