(ns menard.reflexives
  (:require
   [dag_unify.core :as u :refer [unify]]))

(def reflexive-options
  (concat
   [
    {:cat :adjective}
    {:cat :adverb}
    {:cat :conjunction}
    {:cat :intensifier}
    {:cat :noun}
    {:cat :prep}

    {:cat :verb
     :sem {:obj :unspec}}

    ;; reflexive case:
    (let [ref (atom :top)]
      {:cat :verb
       :reflexive true
       :sem {:subj {:ref ref}
             :obj {:ref ref}}})]

   ;; nonreflexive case: we force the subj and obj's
   ;; :refs to be to be distinct from each other.
   ;; In addition, if :subj's :person is :1st or :2nd,
   ;; prevent :obj's person from being the same.
   ;; i.e. prevent non-reflexive "I see me" or
   ;; "you see you":
   (map (fn [x]
          (unify
           {:cat :verb
            :reflexive false
            :sem {:subj {:ref {::is-subj true}}
                  :obj {:ref {::is-subj false}}}}
           x))
        [{:sem {:subj {:person :1st}
                ::match-got-here 1
                 :obj {:person-not :1st}}}
         {:sem {:subj {:person :2nd}
                ::match-got-here 2
                :obj {:person-not :2nd}}}
          {:sem {::match-got-here 3
                 :subj {:person :3rd}}}])))


