(ns menard.test.translate
  (:require [menard.english :as en]
            [menard.nederlands :as nl]
            [menard.translate.spec :refer [nl-to-en-spec]]
            [dag_unify.core :as u]
            [dag_unify.serialization :refer [serialize]]
            [clojure.test :refer [deftest is]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

(defn nl-to-en-str [nl-str]
  (-> nl-str nl/parse first nl-to-en-spec en/generate en/morph))

(deftest nodig
  (log/info (str "nodig tests.."))
  (is (= (nl-to-en-str "ik heb het geld nodig")
         "I need the money"))
  (is (= (nl-to-en-str "ze hebben het geld nodig")
         "they need the money")))

(deftest pronoun-nodig
  (log/info (str "nodig+pronoun tests.."))
  (is (= (nl-to-en-str "jij hebt hun nodig") "you 🤠 need them"))
  (is (= (nl-to-en-str "zij heeft zich nodig") "she needs herself"))
  (is (= (nl-to-en-str "wij hebben ons nodig") "we need ourselves"))
  (is (= (nl-to-en-str "u hebt u nodig") "you 🧐 need yourself")))

;; If true, generates Dutch, then parses it, so we test
;; parsing as well as generation.
(def intermediate-parsing? true)

;; for additional debugging
(def show-english-spec? false)

(defn transfer-fn [i model]
  (let [model-name (:name model "untitled")
        generate (fn [spec]
                   (binding [menard.generate/allow-lexeme-backtracking? true]
                     (nl/generate spec model)))]
    (->> (-> nl/expressions
             (nth i)
             ((fn [spec]
                (log/debug (str "trying to generate Dutch expression spec: " spec " with "
                                "model: " model-name))
                spec))
             generate
             ((fn [generated]
                (is (not (nil? generated)))
                generated))
             ((fn [structure]
                {:structure structure
                 :surface (nl/morph structure)}))
             ((fn [{surface :surface
                    structure :structure}]
                (log/info (str surface " : " (nl/syntax-tree structure)))
                (if intermediate-parsing?
                  (-> (->> (nl/parse surface)
                           ;; remove partial parses, if any:
                           (filter #(not (= true (:menard.parse/partial? %))))
                           ;; prefer parses where subcat is empty e.g. noun phrases rather than nbars:
                           (sort (fn [x y] (and (vector? (u/get-in x [:subcat])) (empty? (u/get-in x [:subcat])))))
                           ;; and take only one parse to test against:
                           (take 1))
                      first
                      ((fn [parsed-structure]
                         (if (not (nil? parsed-structure))
                           {:surface surface
                            :syntax-tree (nl/syntax-tree structure)
                            :structure parsed-structure}
                           {:surface surface
                            :syntax-tree (nl/syntax-tree structure)
                            :structure structure}))))
                  ;; intermediate-parsing? is false:
                  {:surface surface
                   :syntax-tree (nl/syntax-tree structure)
                   :structure structure})))
             ((fn [{surface :surface
                    structure :structure
                    syntax-tree :syntax-tree}]
                (log/debug (str "checking: " (clojure.string/join "," (map nl/syntax-tree structure))))
                (if (nil? structure)
                  (log/warn (str "couldn't parse: " syntax-tree)))
                (is (not (nil? structure)))
                structure)))
         (map (fn [tree] {:nl-st (nl/syntax-tree tree)
                          :sem (u/get-in tree [:sem])
                          :nl (nl/morph tree) :en-spec (nl-to-en-spec tree)}))
         (map (fn [{nl :nl nl-st :nl-st en-spec :en-spec sem :sem}]
                (log/debug (str "nl-st: " nl-st))
                {:nl-st nl-st
                 :sem sem
                 :nl nl
                 :en-spec en-spec
                 :en (-> en-spec en/generate)
                 }))
         (filter (fn [{en :en}] (not (nil? en))))
         (take 1)
         (map (fn [{nl :nl en :en en-spec :en-spec nl-st :nl-st sem :sem}]
                (let [en-st (en/syntax-tree en)
                      en (en/morph en)
                      retval {:i i
                              :sem sem
                              :en-spec en-spec
                              :model (:name model)
                              :nl (str "\"" nl "\"")
                              :nl-st (str "\"" nl-st "\"")
                              :en (str "\"" en "\"")}
                      retval (if show-english-spec?
                               (assoc retval :en-spec (serialize en-spec))
                               retval)]
                  (log/info (str i ": " nl " -> " en))
                  (is (seq nl))
                  (is (seq en))
                  retval)))
         doall)))

(if nl/create-basic-model?
  (deftest transfer-basic
    (->>
     (range 0 (count nl/expressions))
     (map (fn [i]
            (println (str "transfering with nl/expression number: " i))
          (doall
           (take 10
                 (repeatedly #(transfer-fn i @nl/basic-model))))))
     doall)))

(deftest transfer
  (->>
   (range 0 (count nl/expressions))
   (map (fn [i]
          (doall
           (take 10
                 (repeatedly #(transfer-fn i @nl/model))))))
   doall))
