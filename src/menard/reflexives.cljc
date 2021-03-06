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

    ;; intransitive verbs "ik zie"
    {:cat :verb
     :sem {::refl-match 1
           :obj :none}}

    ;; reflexive case (non modal): "ik zie me"
    (let [ref (atom :top)]
      {:cat :verb
       :modal false
       :reflexive true
       :sem {::refl-match 2
             :subj {:ref ref}
             :obj {:ref ref
                   :obj :none}}})

    ;; reflexive case (modal) e.g. "ik probeer me te zien"
    (let [ref (atom :top)]
      {:cat :verb

       ;; TODO: language-dependent: only Dutch has :te value for :modal.
       :modal :te

       :reflexive true
       :sem {::refl-match 3
             :subj {:ref ref}
             :obj {:obj {:ref ref
                         :obj :none}}}})]

   ;; nonreflexive cases: we force the subj's :ref and obj's
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
        [{:agr {:person :1st}
          :sem {:subj {:person :1st}
                ::refl-match 4
                :obj {:person-not :1st
                      :obj :none}}}
         {:agr {:person :1st}
          :sem {:subj {:person :1st}
                ::refl-match 5
                :obj {:obj {:person-not :1st}}}}
         {:agr {:person :2nd}
          :sem {:subj {:person :2nd}
                ::refl-match 6
                :obj {:person-not :2nd
                      :obj :none}}}
         {:agr {:person :2nd}
          :sem {:subj {:person :2nd}
                ::refl-match 7
                :obj {:obj {:person-not :2nd}}}}
         ;; existential? false to
         ;; exclude "er is een hond" from matching here.
         ;; instead it matches above with {::refl-match 1}.
         ;; We don't need to do the other :persons like :1st and :2nd
         ;; because the "er" pronoun is: {:person :3rd}.
         {:agr {:person :3rd}
          :sem {::refl-match 8
                :subj {:person :3rd
                       :existential? false}}}])))
