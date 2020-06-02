(ns babylon.sharing2)

(comment
  :def "cons-and-nest"

  :sem [[1]
        {:mod {:first [[2] :top],
               :rest [[3] :top]}}],
  :mods-nested? true,
  :comp
  {:mods-nested? true,
   :sem [2],
   :head-sem [[4] :top],
   :head-mod [3],
   :parent-sem [1]},
  :head {:sem [4],
         :mods-nested? false,
         :mod [3]}

  "


")
(def cons-and-nest
  (let [two (atom :top)
        three (atom :top)
        one (atom {:mod {:first two
                         :rest three}})
        four (atom :top)]
    {:sem one
     :mods-nested? true
     :comp {:mods-nested? true
            :sem two
            :head-sem four
            :head-mod three
            :parent-sem one}
     :head {:sem four
            :mods-nested? false
            :mod three}}))

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

