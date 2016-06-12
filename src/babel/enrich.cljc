(ns babel.enrich
  (:refer-clojure :exclude [get-in])
  (:require
   [dag_unify.core :refer [fail? get-in strip-refs unify]]
   #?(:clj [clojure.tools.logging :as log])
   #?(:cljs [babel.logjs :as log])))

(declare against-comp)
(declare against-pred)
(declare matching-comp-lexemes)
(declare matching-head-lexemes)

(defn enrich [spec lexicon]
  (let [pred (get-in spec [:synsem :sem :pred] :top)]
    (log/debug (str "against-pred with spec: " spec " and pred: " pred))
    (if (= :top pred)
      spec
      (filter (fn [result]
                (not (fail? result)))
              (map (fn [lexeme]
                     (unify spec
                            {:synsem {:sem (strip-refs (get-in lexeme [:synsem :sem] :top))}}))
                   (filter (fn [lexeme]
                             (= pred
                                (get-in lexeme [:synsem :sem :pred] :top)))
                           (reduce concat (vals lexicon))))))))

    
