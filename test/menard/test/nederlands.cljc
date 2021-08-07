(ns menard.test.nederlands
  (:require [menard.nederlands :as nl :refer [analyze basic-model create-basic-model?
                                              expressions generate load-model morph morphology parse syntax-tree]]
            [menard.morphology :refer [morph-leaf]]
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

  (if create-basic-model?
    (is (= "het oude huis"
           (morph (generate {:cat :noun
                             :subcat []
                             :root "huis"
                             :agr {:number :sing}
                             :sem {:quant :the
                                 :mod {:first {:pred :old
                                               :mod []}
                                       :rest []}}}
                            basic-model)))))

  (is (= "een oud huis"
         (morph (generate {:cat :noun
                           :subcat []
                           :root "huis"
                           :agr {:number :sing}
                           :sem {:quant :some
                                 :mod {:first {:pred :old
                                               :mod []}
                                       :rest []}}}))))

  (if create-basic-model?
    (is (= "een oud huis"
           (morph (generate {:cat :noun
                             :subcat []
                             :root "huis"
                             :agr {:number :sing}
                             :sem {:quant :some
                                   :mod {:first {:pred :old
                                                 :mod []}
                                         :rest []}}}
                            basic-model)))))


  (is (= "de oude huizen"
         (morph (generate {:cat :noun
                           :subcat []
                           :root "huis"
                           :agr {:number :plur}
                           :sem {:quant :the
                                 :mod {:first {:pred :old
                                               :number? false
                                               :mod []}
                                       :rest []}}}))))


  (if create-basic-model?
    (is (= "de oude huizen"
           (morph (generate {:cat :noun
                             :subcat []
                             :root "huis"
                             :agr {:number :plur}
                             :sem {:quant :the
                                   :mod {:first {:pred :old
                                                 :number? false
                                                 :mod []}
                                         :rest []}}}
                            basic-model))))))

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

(deftest parse-test-2
  (is (not (empty?
            (->> "ik heb het nodig"
                 parse
                 (map (fn [x]
                        (log/info (str (syntax-tree x))))))))))

(deftest noun-morphology
  ;; access all morphological rules for Dutch:
  (is (= "zeeën"
         (->
          {:cat :noun
           :null? false
           :agr {:number :plur}
           :canonical "zee"}
          (morph-leaf morphology))))
  (is (= "honden"
         (->
          {:cat :noun
           :null? false
           :agr {:number :plur}
           :canonical "hond"}
          (morph-leaf morphology))))
  (is (= "opdrachten"
         (->
          {:cat :noun
           :null? false
           :agr {:number :plur}
           :canonical "opdracht"}
          (morph-leaf morphology)))))

(deftest present-morphology
  (is (= "zingt"
         (->
          {:canonical "zingen"
           :cat :verb
           :infl :present
             :agr {:number :sing
                   :person :3rd}}
          (morph-leaf morphology))))
  (is (= "uitgelegd"
         (->
          {:canonical "uitgeleggen"
           :cat :verb
           :infl :present
           :agr {:number :sing
                 :person :3rd}}
          (morph-leaf morphology)))))

(deftest past-simple-morphology
  (is (= "huilde"
         (->
          {:canonical "huilen"
           :strong? false
           :cat :verb
           :infl :past-simple
           :agr {:number :sing
                 :person :3rd}}
          (morph-leaf morphology))))
  (is (= "wasten"
         (->
          {:canonical "wassen"
           :strong? false
           :cat :verb
           :infl :past-simple
           :agr {:number :plur
                 :person :3rd}}
          (morph-leaf morphology)))))

(deftest past-simple-strong-morphology-a-oe-a
  (is (= "droeg"
         (->
          {:canonical "dragen"
           :strong? :a-oe-a
           :cat :verb
           :infl :past-simple
           :agr {:number :sing
                 :person :3rd}}
          (morph-leaf morphology))))
  (is (= "droegen"
         (->
          {:canonical "dragen"
           :strong? :a-oe-a
           :cat :verb
           :infl :past-simple
           :agr {:number :plur
                 :person :3rd}}
          (morph-leaf morphology)))))

(deftest past-simple-strong-morphology-a-ie-a
  (is (= "blies"
         (->
          {:canonical "blazen"
           :strong? :a-ie-a
           :cat :verb
           :infl :past-simple
           :agr {:number :sing
                 :person :3rd}}
          (morph-leaf morphology))))
  (is (= "bliezen"
         (->
          {:canonical "blazen"
           :strong? :a-ie-a
           :cat :verb
           :infl :past-simple
           :agr {:number :plur
                 :person :3rd}}
          (morph-leaf morphology))))
  (is (= "liet"
         (->
          {:canonical "laten"
           :strong? :a-ie-a
           :cat :verb
           :infl :past-simple
           :agr {:number :sing
                 :person :3rd}}
          (morph-leaf morphology))))
  (is (= "lieten"
         (->
          {:canonical "laten"
           :strong? :a-ie-a
           :cat :verb
           :infl :past-simple
           :agr {:number :plur
                 :person :3rd}}
          (morph-leaf morphology)))))

(deftest past-simple-strong-morphology-e-a-e
  (is (= "at"
         (->
          {:canonical "eten"
           :strong? :e-a-e
           :cat :verb
           :infl :past-simple
           :agr {:number :sing
                 :person :3rd}}
          (morph-leaf morphology))))
  (is (= "aten"
         (->
          {:canonical "eten"
           :strong? :e-a-e
           :cat :verb
           :infl :past-simple
           :agr {:number :plur
                 :person :3rd}}
          (morph-leaf morphology)))))

(deftest past-simple-strong-morphology-e-o-o
  (is (= "bedolf"
         (->
          {:canonical "bedelven"
           :strong? :e-o-o
           :cat :verb
           :infl :past-simple
           :agr {:number :sing
                 :person :3rd}}
          (morph-leaf morphology))))
  (is (= "bedolven"
         (->
          {:canonical "bedelven"
           :strong? :e-o-o
           :cat :verb
           :infl :past-simple
           :agr {:number :plur
                 :person :3rd}}
          (morph-leaf morphology))))
  (is (= "borg"
         (->
          {:canonical "bergen"
           :strong? :e-o-o
           :cat :verb
           :infl :past-simple
           :agr {:number :sing
                 :person :3rd}}
          (morph-leaf morphology))))
  (is (= "borgen"
         (->
          {:canonical "bergen"
           :strong? :e-o-o
           :cat :verb
           :infl :past-simple
           :agr {:number :plur
                 :person :3rd}}
          (morph-leaf morphology)))))

(deftest past-simple-strong-morphology-i-o-o
  (is (= "vond"
         (->
          {:canonical "vinden"
           :strong? true
           :cat :verb
           :infl :past-simple
           :agr {:number :sing
                 :person :3rd}}
          (morph-leaf morphology))))
  (is (= "vonden"
         (->
          {:canonical "vinden"
           :strong? true
           :cat :verb
           :infl :past-simple
           :agr {:number :plur
                 :person :3rd}}
          (morph-leaf morphology)))))

(deftest past-simple-strong-morphology-ij-ee-e
  (is (= "keek"
         (->
          {:canonical "kijken"
           :strong? true
           :cat :verb
           :infl :past-simple
           :agr {:number :sing
                 :person :3rd}}
          (morph-leaf morphology))))
  (is (= "keken"
         (->
          {:canonical "kijken"
           :strong? true
           :cat :verb
           :infl :past-simple
           :agr {:number :plur
                 :person :3rd}}
          (morph-leaf morphology)))))

(deftest parsing-tests
  (is
   (= ["[s(:present-simple){-} .Corona +[vp-slash-object{-} +[modal+subject(:present-simple){-} +moeten .wij] .[adverb-vp{-} .samen +bestrijden]]]"]
      (->> "Corona moeten wij samen bestrijden" nl/parse (map nl/syntax-tree) (take 1))))
  (is
   (= ["[s(:present-simple){-} .ik +[vp-modal-np(:present-simple){-} +probeer .[vp-np(:infinitive){-} .honden +[vp-te{-} +te .zien]]]]"]
      (->> "ik probeer honden te zien" nl/parse (map nl/syntax-tree) (take 1))))
  (is
   (= ["[s(:present-simple){-} .ik +[vp-modal-te(:present-simple){-} +probeer .[vp-te +te .zien]]]"]
      (vec (->> "ik probeer te zien" nl/parse (map nl/syntax-tree) (take 1))))))

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
