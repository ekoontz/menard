(ns babel.francais.morphology.adjectives)

(def irregular-patterns
  [
   {:path [:français :masc :plur]
    :merge-fn
    (fn [val]
      {:français {:agr {:gender :masc
                        :number :plur}}})}
   {:path [:français :fem :plur]
    :merge-fn
    (fn [val]
      {:français {:agr {:gender :fem
                        :number :plur}}})}
   ])
