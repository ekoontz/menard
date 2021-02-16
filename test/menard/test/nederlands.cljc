(ns menard.test.nederlands
  (:require [menard.nederlands :as nl :refer [analyze basic-model expressions generate load-model morph parse syntax-tree]]
            [dag_unify.core :as u]
            [clojure.test :refer [deftest is]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

(deftest adjective-agreement
  (is (= "het oude huis"
         (morph (generate {:cat :noun
                           :subcat []
                           :root "huis"
                           :agr {:number :sing}
                           :sem {:quant :the
                                 :mod {:first {:pred :old
                                               :mod []}
                                       :rest []}}}))))

  (is (= "het oude huis"
         (morph (generate {:cat :noun
                           :subcat []
                           :root "huis"
                           :agr {:number :sing}
                           :sem {:quant :the
                                 :mod {:first {:pred :old
                                               :mod []}
                                       :rest []}}}
                          basic-model))))

  (is (= "een oud huis"
         (morph (generate {:cat :noun
                           :subcat []
                           :root "huis"
                           :agr {:number :sing}
                           :sem {:quant :some
                                 :mod {:first {:pred :old
                                               :mod []}
                                       :rest []}}}))))

  (is (= "een oud huis"
         (morph (generate {:cat :noun
                           :subcat []
                           :root "huis"
                           :agr {:number :sing}
                           :sem {:quant :some
                                 :mod {:first {:pred :old
                                               :mod []}
                                       :rest []}}}
                          basic-model))))


  (is (= "de oude huizen"
         (morph (generate {:cat :noun
                           :subcat []
                           :root "huis"
                           :agr {:number :plur}
                           :sem {:quant :the
                                 :mod {:first {:pred :old
                                               :mod []}
                                       :rest []}}}))))


  (is (= "de oude huizen"
         (morph (generate {:cat :noun
                           :subcat []
                           :root "huis"
                           :agr {:number :plur}
                           :sem {:quant :the
                                 :mod {:first {:pred :old
                                               :mod []}
                                       :rest []}}}
                          basic-model)))))

(def generate-per-expression 5)

;; exclude non-production expressions:
(def prod-expressions (->> expressions
                      (filter #(= true (u/get-in % [:prod?] true)))))

(deftest all-expressions-work-1
  (let [expression-sets
        (->>
         (range 0 (count prod-expressions))
         (map (fn [index]
                (take generate-per-expression
                      (repeatedly #(or (generate (nth expressions index))
                                       ;; if generation fails, save the :note
                                       ;; so we can see where the fail happened in the
                                       ;; log/info messages printed below.
                                       {:note (-> expressions (nth index) :note)}))))))]
    ;; generation test 1
    (is (= (count prod-expressions)
           (count expression-sets)))))

;; generation test 2
(deftest all-expressions-work-2
  (let [expression-sets
        (->>
         ;;         (range 0 (count prod-expressions))
         (range 18 19)
         (map (fn [index]
                (take generate-per-expression
                      (repeatedly #(or (generate (nth expressions index))
                                       ;; if generation fails, save the :note
                                       ;; so we can see where the fail happened in the
                                       ;; log/info messages printed below.
                                       {:note (-> expressions (nth index) :note)}))))))]
    (is (empty?
         (->> expression-sets
              (map (fn [expression-set]
                     (->> expression-set
                          (map (fn [expression]
                                 (log/info (str (-> expression :note) ": generate: '"
                                                (morph expression) "'"))
                                 (morph expression)))
                          (remove (fn [expression]
                                    (nil? (clojure.string/index-of (morph expression) \_)))))))
              (remove empty?))))))

;; generation test 3
(deftest all-expressions-3
  (let [expression-sets
        (->>
         (range 0 (count prod-expressions))
         (map (fn [index]
                (take generate-per-expression
                      (repeatedly #(or (generate (nth expressions index))
                                       ;; if generation fails, save the :note
                                       ;; so we can see where the fail happened in the
                                       ;; log/info messages printed below.
                                       {:note (-> expressions (nth index) :note)}))))))]
    (is (empty?
         (->> expression-sets
              (map (fn [expression-set]
                     (->> expression-set
                          (map (fn [expression]
                                 (log/info (str (-> expression :note) ": generate: '"
                                                (morph expression) "'")))))))
              ;; count how many expressions we generated:
              (map count)

              ;; remove all cases where we generated enough expressions. Afterwards, if the remaining is empty,
              ;; the test passes:
              (remove #(= % generate-per-expression)))))))

(deftest parse-test-1
  (let [expression-sets
        (->>
         (range 0 (count prod-expressions))
         (map (fn [index]
                (take generate-per-expression
                      (repeatedly #(or (generate (nth expressions index))
                                       ;; if generation fails, save the :note
                                       ;; so we can see where the fail happened in the
                                       ;; log/info messages printed below.
                                       {:note (-> expressions (nth index) :note)}))))))]
    ;; parse test 1
    (is (empty?
         (->> expression-sets
              (map (fn [expression-set]
                     (->> expression-set
                          (map (fn [expression]
                                 (log/info (str (-> expression :note) ": '" (morph expression) "': parse: "
                                                (-> expression morph parse first syntax-tree))))))))
              (remove #(not (empty? %))))))))

(deftest morphology
  ;; access all morphological rules for Dutch:
  (let [morphology (-> menard.nederlands/model deref :morphology)]

    (is (= "zeeën"
           (menard.morphology/morph-leaf
            {:cat :noun
             :null? false
             :agr {:number :plur}
             :canonical "zee"} morphology)))
    (is (= "honden"
           (menard.morphology/morph-leaf
            {:cat :noun
             :null? false
             :agr {:number :plur}
             :canonical "hond"} morphology)))
    (is (= "opdrachten"
           (menard.morphology/morph-leaf
            {:cat :noun
             :null? false
             :agr {:number :plur}
             :canonical "opdracht"} morphology)))

    (is (= "zingt"
           (->
            {:canonical "zingen"
             :cat :verb
             :infl :present
             :agr {:number :sing
                   :person :3rd}}
            (menard.morphology/morph-leaf morphology))))
            
    (is (= "uitgelegd"
           (->
            {:canonical "uitgeleggen"
             :cat :verb
             :infl :present
             :agr {:number :sing
                   :person :3rd}}
            (menard.morphology/morph-leaf morphology))))

    (is (= "huilde"
           (->
            {:canonical "huilen"
             :cat :verb
             :infl :past-simple
             :agr {:number :sing
                   :person :3rd}}
            (menard.morphology/morph-leaf morphology))))

    (is (= "wasten"
           (->
            {:canonical "wassen"
             :cat :verb
             :infl :past-simple
             :agr {:number :plur
                   :person :3rd}}
            (menard.morphology/morph-leaf morphology))))))

(deftest parsing-tests
  (is
   (= ["[s(:present-simple) .Corona +[vp-slash-object +[modal+subject(:present-simple) +moeten .wij] .[adverb-vp .samen +bestrijden]]]"]
      (->> "Corona moeten wij samen bestrijden" nl/parse (map nl/syntax-tree) (take 1))))
  (is
   (= ["[s(:present-simple) .ik +[vp-modal-np(:present-simple) +probeer .[vp-np(:infinitive) .honden +[vp-te +te .zien]]]]"]
      (->> "ik probeer honden te zien" nl/parse (map nl/syntax-tree))))
  (is
   (= ["[s(:present-simple) .ik +[vp-modal-te(:present-simple) +probeer .[vp-te +te .zien]]]"]
      (->> "ik probeer te zien" nl/parse (map nl/syntax-tree)))))


;; If true, generates Dutch, then parses it, so we test
;; parsing as well as generation.
(def intermediate-parsing? true)

(defn validator
  "example usage: 
  (validator (nth nl/expressions 15))))))"
  [spec & [times]]
  (let [times (or times 10)]
    (count (take
            times
            (repeatedly
             #(-> spec
                  nl/generate
                  ((fn [x]
                     (if (nil? x)
                       (menard.exception/exception "failed to generate.") x)))
                  ((fn [x]
                     (if intermediate-parsing?
                       (-> x nl/morph nl/parse first)
                       x)))
                  nl/syntax-tree
                  println))))))

(deftest validations
  (doall
   (->>
    (range 0 (count nl/expressions))
    (map (fn [x]
           (is (= 10 (menard.test.nederlands/validator (nth nl/expressions x)))))))))
