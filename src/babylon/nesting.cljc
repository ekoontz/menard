(ns babylon.nesting)

;; These are rules for sharing
;; contents of various word- or phrase-
;; level maps, when nesting applies
;; (i.e. when the :mod of the head's :sem is nested
;;  within the parent's :sem).




(comment
  :def "cons-and-nest"
  :sem {:mod {:first [[1] :top]
              :rest [[2] :top]}}
  :mods-nested? true
  :comp {:mods-nested? true
         :sem [1]}
  :head {:mods-nested? false
         :mod [2]}
  "used for the top-most adjunct in a phrase,
   e.g. in '[.vier [.kleine +honden]]', 
   the top-most adjunct is 'vier'."
  )
(def cons-and-nest
  (let [one (atom :top)
        two (atom :top)]
    {:sem {:mod {:first one
                 :rest two}}
     :mods-nested? true
     :comp {:mods-nested? true
            :sem one}
     :head {:mods-nested? false
            :mod two}}))

(comment
  :mods-nested? false
  :mod {:first [[1] :top]
        :rest [[2] :top]}
  :comp {:mods-nested? true
         :sem [1]}
  :head {:mod [2]
         :mods-nested? false}
  "add a comment here..")
(def cons-only
  (let [one (atom :top)
        two (atom :top)]
    {:mods-nested? false
     :mod {:first one
           :rest two}
     :comp {:mods-nested? true
            :sem one}
     :head {:mod two
            :mods-nested? false}}))

(def nest-only
  (let [two (atom :top)
        three (atom :top)
        four (atom :top)]
    {:mods-nested? true
     :sem {:mod two}
     :comp {:mods-nested? true
            :head-sem three
            :parent-sem four}
     :head {:sem three
            :mods-nested? false
            :mod two}}))

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
