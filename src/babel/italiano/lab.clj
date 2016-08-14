(in-ns 'babel.italiano)

(defn in-front-of []
  (repeatedly #(let [result (try (generate {:synsem {:sem {:pred :in-front-of
                                                           :subj {:pred :table}
                                                           :obj {:pred :easy-chair}}
                                                     :cat :verb}}
                                           :truncate-children false)
                                               (catch Exception e (log/error (str "failed to generate."))))]
                 (if result
                   (println (fo result))))))
