(ns babylon.english.poetry
  (:require
   [babylon.english :as en :refer [analyze generate grammar index-fn lexicon
                                   morph parse syntax-tree]]
   [babylon.generate :as g :refer [add lazy-map]]
   [dag_unify.core :as u]
   [clojure.tools.logging :as log]))

(def wh-q-1
  "What did she hear?"
  {:rule "s-wh-interog"
   :head {:rule "s-interog-slash"
          :comp {:rule "s-comp-2"
                 :head {:phrasal false
                        :subcat {:2 {:cat :noun}}}
                 :comp {:phrasal false}}
          :head {:phrasal false}}
   :comp {:phrasal false}})

(def wh-q-2
  "At who does she look?"
  {:rule "s-wh-interog"
   :head {:rule "s-interog-slash"
          :comp {:rule "s-comp-2"
                 :head {:phrasal false
                        :canonical "look"
                        :subcat {:2 {:cat :prep}}}}
          :head {:phrasal false}}})

(def wh-q-3
  "What did they look at?"
  {:rule "s-wh-interog"
   :comp {:phrasal false}
   :head {:rule "s-interog-slash"
          :comp {:rule "s-comp-2"
                 :head {:rule "vp-slash"}}
          :head {:phrasal false}}})

(def medium-declarative
  "the social students would leave the major rooms"
  (-> {:rule "s"
       :comp {:rule "np"
              :phrasal true
              :head {:rule "nbar"}}
       :head {:rule "vp-aux"
              :comp {:rule "vp"
                     :comp {:rule "np"
                            :phrasal true
                            :head {:rule "nbar"}}}}}))
(def medium-declarative-2
  "mothers eat the things that look"
  {:rule "s"
   :comp {:phrasal false}
   :head {:phrasal true
          :rule "vp"
          :head {:phrasal false}
          :comp {:phrasal true
                 :rule "np"
                 :head {:phrasal true
                        :rule "nbar3"
                        :comp {:phrasal true
                               :rule "comp1"
                               :head {:phrasal false}
                               :comp {:phrasal false}}}}}})
(def that-spec
  {:rule "s"
   :head {:rule "vp"
          :head {:phrasal false
                 :subcat {:2 {:cat :comp
                              :rule "comp2"}}}}})

;; Currently we can generate in 2.1 secs at 50% median, 2.4 secs max.
(def long-declarative
  "The old students walk hands that the good studies would teach."
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
                                             :subcat {:1 {:cat :noun}}}}}}}}})

(def poetry-specs
  [long-declarative
   medium-declarative
   medium-declarative-2
   that-spec
   wh-q-1
   wh-q-2
   wh-q-3])

(defn poetry-line []
  (try
    (->
     poetry-specs
     shuffle
     first
     generate)
    (catch Exception e
      (log/warn (str "fail: " e)))))

(defn good-poetry []
  (loop []
    (println (morph (generate (first poetry-specs))))
    (recur)))

(defn poetry []
  (loop []
    (println (morph (or (poetry-line) "(failed)") :sentence-punctuation? true))
    (recur)))

(def consumer-patience 6000)

(defn timeout-with [timeout-ms callback]
   (let [fut (future (callback))
         ret (deref fut timeout-ms ::timed-out)]
     (when (= ret ::timed-out)
       (println (str "Too bad! you took more than " timeout-ms " msecs long."))
       (future-cancel fut))
     ret))

(defn generate-with-timeout []
    (println (str "Generating.."))
    ((or morph syntax-tree morph) (poetry-line)))

(defn poetry-line-with-timeout []
  (let [result (timeout-with consumer-patience generate-with-timeout)]
    (if (= ::timed-out result)
      (do
        (println (str "retrying.."))
        (poetry-line-with-timeout))
      result)))

(defn poetry-with-timeout []
  (take 50 (repeatedly #(println (time (poetry-line-with-timeout))))))

