(ns babylon.semantic-contents)

;; These are rules for sharing
;; contents of various word- or phrase-
;; level maps, when nesting applies
;; (i.e. when the :mod of the head's :sem is nested
;;  within the parent's :sem).

;; Needed so that adjuncts can modify
;; the semantics of heads.
;; The :sem of the head is distinct from
;; the :sem of the parent, but all of the
;; contents within the :sem *are* shared
;; between head and parent.
;; The parent's :sem therefore has all of the
;; contents of the :head's sem, plus the :mod
;; of the complement.
(def noun
  (let [one (atom :top)
        two (atom :top)
        three (atom :top)
        four (atom :top)]
    {:sem {:pred one
           :quant two
           :ref three
           :context four}
     :head {:sem {:pred one
                  :quant two
                  :ref three
                  :context four}}}))

(def adjective
  (let [one (atom :top)]
    {:sem {:pred one}
     :head {:sem {:pred one}}}))
