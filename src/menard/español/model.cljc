(ns menard.español.model
  (:require [dag_unify.core :as u]
            [clojure.tools.logging :as log]
            [menard.español :as es]
            [menard.español.curated-verbs :as curated]
            [menard.model :refer [create] :as model]
            [menard.serialization :as s]))

(defn- filter-lexicon [curated-lex verbs-from-set]
  (->> (keys curated-lex) 
       (map (fn [k] (let [filtered-values (->>
                                           (get curated-lex k)
                                           (filter #(or
                                                     (contains? verbs-from-set k)
                                                     (= true (u/get-in % [:aux?]))
                                                     (not (= :verb (u/get-in % [:cat]))))))]
                      (if (seq filtered-values)
                        [k filtered-values]))))
       (filter #(not (nil? %)))))

(defn create-target-model-from-verbs [verbs]
  (-> (merge @curated/model
             {:indices (model/fill-lexicon-indexes
                        (into {} (filter-lexicon (:lexicon @curated/model) (->> verbs (map :canonical) set))))
              :name (str (:name @curated/model) " filtered with " (count verbs) " verb" (when (not (= (count verbs) 1)) "s"))})
      (model/add-functions true)
      ref))


