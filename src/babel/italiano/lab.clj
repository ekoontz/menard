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

(def foo 
  (babel.italiano.grammar/model
   (fn [val] (or (and (= :verb (get-in val [:synsem :cat]))
                      (= :top (get-in val [:synsem :infl])))
                 (= true (get-in val [:synsem :aux] false))
                 (= true (get-in val [:synsem :pronoun] false))))
   (fn [rule] true)))

(defn bar []
  (map fo-ps (filter #(not (dag_unify.core/fail? %))
                     (mapcat (fn [lexeme]
                               (babel.over/overc (first (:grammar foo))
                                                 lexeme))
                             (vals (:lexicon foo))))))

(def stats-for-rules
  (map (fn [rule] 
         {:lexical-comp-count (count (filter #(not (dag_unify.core/fail? %))
                                             (mapcat (fn [lexeme]
                                                       (babel.over/overc rule
                                                                         lexeme))
                                                     (vals (:lexicon foo)))))
          :lexical-head-count
          (count (filter #(not (dag_unify.core/fail? %))
                         (mapcat (fn [lexeme]
                                   (babel.over/overh rule
                                                     lexeme))
                                 (vals (:lexicon foo)))))
          :phrasal-head-count
          (count (filter #(not (dag_unify.core/fail? %))
                         (mapcat (fn [child-rule]
                                   (if (not (= child-rule rule))
                                     (babel.over/overh rule
                                                       child-rule)))
                                 (:grammar foo))))
          :phrasal-comp-count
          (count (filter #(not (dag_unify.core/fail? %))
                         (mapcat (fn [child-rule]
                                   (if (not (= child-rule rule))
                                     (babel.over/overc rule
                                                       child-rule)))
                                 (:grammar foo))))
          :rule (:rule rule)
          :phrasal-comp? (get-in rule [:comp :phrasal] true)
          :phrasal-head? (get-in rule [:head :phrasal] true)})
       (:grammar foo)))



