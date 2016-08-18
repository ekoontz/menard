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

(defn sleep []
  (repeatedly #(println (fo (generate {:synsem {:cat :verb
                                                             :essere false
                                                :sem {:pred :sleep
                                                      :tense :conditional}}}
                                      :truncate false
                                      :model medium)))))

(defn nps []
  (repeatedly #(println (fo (generate {:synsem {:cat :noun}}
                                      :truncate false
                                      :model medium)))))
