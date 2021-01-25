(ns menard.test.nederlands
  (:require [menard.nederlands :as nl :refer [analyze expressions generate load-model morph parse syntax-tree]]
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
                                       :rest []}}
                           :training-wheels {:comp {:cat :det}
                                             :sem {:mod {:first {:number? false}}}}}))))
  (is (= "een oud huis"
         (morph (generate {:cat :noun
                           :subcat []
                           :root "huis"
                           :agr {:number :sing}
                           :sem {:quant :some
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
                                       :rest []}}
                           :training-wheels {:comp {:cat :det}
                                             :sem {:mod {:first {:number? false}}}}})))))

(deftest all-expressions-work
  (let [generate-per-expression 5

        ;; exclude non-production expressions:
        expressions (->> expressions
                         (filter #(= true (u/get-in % [:prod?] true))))

        expression-sets
        (->>
         (range 0 (count expressions))
         (map (fn [index]
                (take generate-per-expression
                      (repeatedly #(or (generate (nth expressions index))
                                       ;; if generation fails, save the :note
                                       ;; so we can see where the fail happened in the
                                       ;; log/info messages printed below.
                                       {:note (-> expressions (nth index) :note)}))))))]

    ;; generation test 1
    (is (= (count expressions)
           (count expression-sets)))

    ;; generation test 2
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
              (remove empty?))))
    
    ;; generation test 3
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
              (remove #(= % generate-per-expression)))))

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
      (->> "Corona moeten wij samen bestrijden" nl/parse (map nl/syntax-tree))))
  (is
   (= ["[s(:present-simple) .ik +[vp-modal-np(:present-simple) +probeer .[vp-np(:infinitive) .honden +[vp-te +te .zien]]]]"]
      (->> "ik probeer honden te zien" nl/parse (map nl/syntax-tree))))
  (is
   (= ["[s(:present-simple) .ik +[vp-modal-te(:present-simple) +probeer .[vp-te +te .zien]]]"]
      (->> "ik probeer te zien" nl/parse (map nl/syntax-tree)))))


;; set this to true once we move reflexive constraint-checking outside of menard.generate,
;; so that parsing can use it too:
(def intermediate-parsing? false)

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
