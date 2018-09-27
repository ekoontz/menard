(ns babel.italiano.morphology.nouns)

(def exceptions-rules
  [{:path [:italiano :plur]
    :merge-fn
    (fn [val]
      {:synsem {:cat :noun}
       :italiano {:agr {:number :plur}}})}])

