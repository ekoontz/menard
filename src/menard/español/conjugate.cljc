(ns menard.español.conjugate
  (:require [menard.español :refer [generate morph]]))

(defn verb [canonical inflection]
  {:canonical canonical
   :inflection inflection
   :singular {:1st (-> {:root canonical
                        :phrasal? true
                        :infl inflection
                        :agr {:person :1st :number :sing}
                        :subcat []
                        :comp {:phrasal? false}}
                       generate
                       morph)
              :2nd-informal (-> {:root canonical
                                 :phrasal? true
                                 :infl inflection
                                 :agr {:person :2nd
                                       :formal? false
                                       :number :sing}
                                 :subcat []
                        :comp {:phrasal? false}}
                                generate
                                morph)
              :2nd-formal (-> {:root canonical
                               :phrasal? true
                               :infl inflection
                               :agr {:person :2nd
                                     :formal? true
                              :number :sing}
                               :subcat []
                               :comp {:phrasal? false}}
                              generate
                              morph)
              :3rd (-> {:root canonical
                        :phrasal? true
                        :infl inflection
                        :agr {:person :3rd
                              :number :sing}
                        :subcat []
                        :comp {:phrasal? false}}
                       generate
                       morph)}})


              

   

