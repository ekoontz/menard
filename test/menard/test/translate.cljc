(ns menard.test.translate
  (:require [menard.english :as en]
            [menard.nederlands :as nl]
            [menard.translate :refer [nl-to-en-spec]]
            [dag_unify.core :as u]
            [dag_unify.serialization :refer [serialize]]
            [clojure.test :refer [deftest is]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

(defn nl-to-en-str [nl-str]
  (->> nl-str nl/parse (take 1) first nl-to-en-spec en/generate en/morph))

(deftest nodig
  (is (= (nl-to-en-str "ik heb het geld nodig")
         "I need the money"))
  (is (= (nl-to-en-str "ze hebben het geld nodig")
         "they need the money")))

(deftest pronoun-nodig
  (is (= (nl-to-en-str "jij hebt hun nodig") "you 🤠 need them"))
  (is (= (nl-to-en-str "zij heeft zich nodig") "she needs herself"))
  (is (= (nl-to-en-str "wij hebben ons nodig") "we need ourselves"))
  (is (= (nl-to-en-str "u hebt u nodig") "you 🧐 need yourself")))

;; set this to true once we move reflexive constraint-checking outside of menard.generate,
;; so that parsing can use it too:
(def intermediate-parsing? false)

;; for additional debugging
(def show-english-spec? false)

(defn transfer-fn [i]
  (->> (if intermediate-parsing?
         (-> nl/expressions (nth i) nl/generate nl/morph nl/parse)
         (-> nl/expressions (nth i) nl/generate list))
       (map (fn [tree] {:nl (nl/morph tree) :en-spec (nl-to-en-spec tree)}))
       (map (fn [{nl :nl en-spec :en-spec}]
              {:nl nl :en-spec en-spec :en (-> en-spec en/generate)}))
       (filter (fn [{en :en}] (not (nil? en))))
       (take 1)
       (map (fn [{nl :nl en :en en-spec :en-spec}]
              (let [en (en/morph en)
                    retval {:i i
                            :nl (str "\"" nl "\"")
                            :en (str "\"" en "\"")}
                    retval (if show-english-spec?
                             (assoc :en-spec (serialize en-spec))
                             retval)]
                (println retval)
                (is (seq nl))
                (is (seq en))
                retval)))
       doall))

(deftest transfer
  (->>
   (range 0 (count nl/expressions))
   (map (fn [i]
          (doall
           (take 5
                 (repeatedly #(transfer-fn i))))))
   doall))












       
  

