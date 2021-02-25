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

    ;; reflexive case (non modal): "ik zie me"
    (let [ref (atom :top)]
      {:cat :verb
       :reflexive true
       :sem {:subj {:ref ref}
             :obj {:ref ref
                   :obj ::unspec}}})

    ;; reflexive case (modal) e.g. "ik probeer me te zien"
    (let [ref (atom :top)]
      {:cat :verb
       :reflexive true
       :sem {:subj {:ref ref}
             :obj {:obj {:ref ref}}}})]

   ;; nonreflexive case: we force the subj's :ref and obj's
   ;; :ref to be to be distinct from each other.
   ;; In addition, if :subj's :person is :1st or :2nd,
   ;; prevent :obj's person from being the same.
   ;; i.e. prevent non-reflexive "I see me" or
   ;; "you see you".
   ;; but for :3rd, "he sees him" is ok, as long as the
   ;; subj :ref and :obj :ref are distinct.
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
                :obj {:person-not :2nd
                      :obj ::unspec}}}
         {:sem {::match-got-here 3
                :subj {:person :3rd}}}])))


