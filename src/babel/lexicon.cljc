(ns babel.lexicon)

;; mapping of predicates: e.g. :say, to lexical implications.
;; e.g., if verb is :say, then :subj (i.e. the speaker) must
;; be human, and :obj (i.e. what is spoken) must be speakable.
;; TODO: consider simplifying this by stipulating that the
;; constraint applies only to {:sem ...},
;; not {:synsem { .. :sem .. }}.
(def universals
  {:say {:synsem {:sem {:subj {:human true}
                        :obj {:speakable true}}}}
   :tell {:synsem {:sem {:subj {:human true}
                         :obj {:human true}}}}})

;; TODO: add other :pred -> constraints, and
;; DRY stuff up by removing the same from language-specific lexicons,
;; where it's present
