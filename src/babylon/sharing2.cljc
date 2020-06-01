(ns babylon.sharing2)

(def cons-and-nest
  (let [one (atom :top)
        two (atom :top)
        three (atom :top)
        four (atom {:first one
                    :rest two})]
    {:sem four
     :comp {:sem one
            :head-sem three
            :parent-sem four}
     :head {:sem three
            :mod two}}))

(def nocons-and-nest
  (let [two (atom :top)
        three (atom :top)
        four (atom :top)]
    {:sem {:mod two}
     :comp {:head-sem three
            :parent-sem four}
     :head {:sem three
            :mod two}}))

(def cons-and-nonest
  (let [one (atom :top)
        two (atom :top)
        four (atom :top)]
    {:mod {:first one
           :rest two}
     :comp {:sem one
            :head-sem four
            :parent-sem four}
     :head {:mod {:first one
                  :rest two}}}))

(def unify-with-this-for-each-of-the-above
  (let [one (atom :top)
        two (atom :top)]
    {:head-sem one
     :parent-sem two
     :head {:head-sem one
            :parent-sem two}}))

