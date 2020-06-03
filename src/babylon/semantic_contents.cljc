(ns babylon.semantic-contents)

;; These are rules for sharing
;; contents of various word- or phrase-
;; level maps.
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
     :head {:pred one
            :quant two
            :ref three
            :context four}}))


