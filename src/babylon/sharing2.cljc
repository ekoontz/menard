(ns babylon.sharing2)

(def cons-and-nest
  (let [one (atom :top)
        two (atom :top)
        three (atom :top)
        four (atom {:mod {:first one
                          :rest two}})]
    {:sem four
     :mods-nested? true
     :comp {:mods-nested? true
            :sem one
            :head-sem three
            :head-mod two
            :parent-sem four}
     :head {:sem three
            :mods-nested? false
            :mod two}}))

(def cons-and-no-nest
  (let [one (atom :top)
        two (atom :top)
        three (atom :top)]
    {:mods-nested? false
     :mod {:first one
           :rest two}
     :comp {:mods-nested? true
            :sem one
            :parent-sem three}
     :head {:mod two
            :mods-nested? false}}))

(def nocons-and-nest
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

(comment
  :def "promote"
  :head-sem [[2] :top],
  :parent-sem [[3] :top],
  :head {:head-mod [1], :head-sem [2], :parent-sem [3]}

  "Complements modify the heads and parents of their phrases by using
   :head-mod, :head-sem, and :parent-sem. This rule 'promote'
   allows a head of a lower phrase to modify, as a complement, a parent and head at
   a higher level. 

   For example, in: 

      [[np .small +dogs] sleep]

   The complement 'dogs' modifies the semantics of the head 'sleep', by
    means of 'dog's features :head-mod,:head-sem, and :parent-sem
    These three features are copied up to [np .small +dogs], so that
    through that np, 'dogs' can modify the semantics of 'sleep'.")
  
(def promote
  (let [one (atom :top)
        two (atom :top)
        three (atom :top)]
    {:head-mod one
     :head-sem two
     :parent-sem three
     :head {:head-mod one
            :head-sem two
            :parent-sem three}}))

