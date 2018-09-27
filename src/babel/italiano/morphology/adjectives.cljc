(ns babel.italiano.morphology.adjectives)

(def exceptions-rules
  [
   ;; adjectives
   {:path [:italiano :masc :plur]
    :merge-fn
    (fn [val]
      {:italiano {:agr {:gender :masc
                        :number :plur}}})}
   
   {:path [:italiano :fem :plur]
    :merge-fn
    (fn [val]
      {:italiano {:agr {:gender :fem
                        :number :plur}}})}
   {:path [:italiano :fem :sing]
    :merge-fn
    (fn [val]
      {:italiano {:agr {:gender :fem
                        :number :sing}}})}])
