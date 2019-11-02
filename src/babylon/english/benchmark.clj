(ns babylon.english.benchmark
  (:require
   [babylon.english :as en :refer [generate morph]]
   [clojure.tools.logging :as log]))

;; Currently we can generate in 1.3 secs at 50p, 1.6 secs max.
(def long-declarative
  "The old students walk hands that the good studies would teach."
  {:rule "s"
   :comp {:rule "np"
          :head {:phrasal true}}
   :head {:rule "vp"
          :head {:phrasal false
                 :subcat {:2 {:cat :comp
                              :rule "comp2"
                              :phrasal true
                              :comp
                              {:rule "s"
                               :comp {:rule "np"
                                      :head {:phrasal true}}
                               :head {:rule "vp"
                                      :head {:subcat {:1 {:cat :noun}
                                                      :2 {:cat :noun}
                                                      :3 []}}
                                      :comp {:rule "nbar4"
                                             :phrasal true
                                             :head {:phrasal false
                                                    :canonical :top}
                                             :comp {:phrasal true
                                                    :rule "comp1"
                                                    :comp {:phrasal true
                                                           :rule "s-slash"
                                                           :comp {:phrasal true
                                                                  :rule "np"
                                                                  :head {:phrasal true}
                                                                  :comp {:phrasal false}}
                                                           :head {:phrasal true
                                                                  :rule "vp-aux-slash"
                                                                  :head {:phrasal false
                                                                         :aux true
                                                                         :subcat {:2 {:modal false}}}
                                                                  :comp {:phrasal (or false false) ;; changing this to true will take a long time and finally you'll get a 'dead end' error.
                                                                         :subcat {:1 {:cat :noun}}}}}}}}}}}}}})

(defn benchmark-line []
  (try
    (->
     long-declarative
     generate
     time)
    (catch Exception e
      (log/warn (str "fail: " e)))))

(defn benchmark []
  (take 20
        (loop []
          (println (morph (or (benchmark-line) "(failed)") :sentence-punctuation? true))
          (recur))))

