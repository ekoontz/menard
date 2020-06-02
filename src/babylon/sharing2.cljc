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

