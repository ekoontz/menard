(ns babylon.sharing)

;; This file consists of sharing that
;; used to be in babylon.ug, but moved out because
;; it looks very language-dependent (e.g. refers to
;; internals of verbs and nouns' semantics).

;; Nest the head's :mod inside the parent's [:sem], and also
;; copy all of the head's :sem keys into the parent's :sem.
(def nest-mod-nominal
  (let [arg (atom :top)
        context (atom :top)
        mod (atom :top)
        pred (atom :top)
        quant (atom :top)
        ref (atom :top)]
    {:sem {:arg arg
           :context context
           :mod mod
           :pred pred
           :quant quant
           :ref ref}
     :head {:sem {:arg arg
                  :context context
                  :pred pred
                  :quant quant
                  :ref ref}
            :mod mod}}))

(def sem-args-shared-with-head
  (let [arg1 (atom :top)
        arg2 (atom :top)]
    {:sem {:arg1 arg1
           :arg2 arg2}
     :head {:sem {:arg1 arg1
                  :arg2 arg2}}}))

(def copy-verbal-head-sem-to-sem
  (let [pred (atom :top)
        subj (atom :top)
        obj (atom :top)
        tense (atom :top)
        aspect (atom :top)]
    {:cat :verb
     :sem {:pred pred
           :aspect aspect
           :tense tense
           :subj subj
           :obj obj}
     :head {:sem {:pred pred
                  :aspect aspect
                  :tense tense
                  :subj subj
                  :obj obj}}}))


(def noun-head-sem-content-is-sem-content
  (let [pred (atom :top)
        ref (atom :top)
        quant (atom :top)
        context (atom :top)]
    {:cat :noun
     :sem {:pred pred
           :ref ref
           :quant quant
           :context context}
     :head {:sem {:pred pred
                  :ref ref
                  :quant quant
                  :context context}}}))

