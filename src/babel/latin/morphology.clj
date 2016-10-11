(ns babel.latin.morphology
  )

(def replace-patterns
  {:p [#"^(.)eo$" "$1ēre"]
   :g [#"^(.)ēre$" "$1eo"]
   :u {:synsem {:infl :present
                :sem {:tense :present}
                :subcat {:1 {:agr {:number :sing
                                   :person :1st}}}}}}
  {:p [#"^(.)as$" "$1ēre"]
   :g [#"^(.)ēre$" "$1as"]
   :u {:synsem {:infl :present
                :sem {:tense :present}
                :subcat {:1 {:agr {:number :sing
                                   :person :2nd}}}}}}
  {:p [#"^(.)at$" "$1ēre"]
   :g [#"^(.)ēre$" "$1at"]
   :u {:synsem {:infl :present
                :sem {:tense :present}
                :subcat {:1 {:agr {:number :sing
                                   :person :3rd}}}}}}
  {:p [#"^(.)amus$" "$1ēre"]
   :g [#"^(.)ēre$" "$1amus"]
   :u {:synsem {:infl :present
                :sem {:tense :present}
                :subcat {:1 {:agr {:number :plur
                                   :person :1st}}}}}}
  {:p [#"^(.)atis$" "$1ēre"]
   :g [#"^(.)ēre$" "$1atis"]
   :u {:synsem {:infl :present
                :sem {:tense :present}
                :subcat {:1 {:agr {:number :plur
                                   :person :2nd}}}}}}
  {:p [#"^(.)ant$" "$1ēre"]
   :g [#"^(.)ēre$" "$1ant"]
   :u {:synsem {:infl :present
                :sem {:tense :present}
                :subcat {:1 {:agr {:number :plur
                                   :person :3rd}}}}}})
   
(defn analyze [surface-form lexicon]
  "Analyze a single surface form into a set of lexical forms."
  )

(defn conjugate [infinitive unify-with]
  "Conjugate an infinitive into a surface form by taking the first 
   element of replace-patterns where the element's :u unifies successfully with
   unify-with."
  (first
   (take 1
         (remove #(nil? %)
                 (map
                  (fn [replace-pattern]
                    (let [from (nth (:g replace-pattern) 0)
                          to (nth (:g replace-pattern) 1)
                          unify-against (or (:u replace-pattern) :top)]
                      (if (and from to
                               (re-matches from infinitive)
                               (not (fail? (unifyc unify-against
                                                   unify-with))))
                        (string/replace infinitive from to))))
                  replace-patterns)))))
