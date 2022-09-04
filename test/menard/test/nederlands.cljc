(ns menard.test.nederlands
  (:require [menard.nederlands :as nl :refer [analyze expressions generate load-model morph parse syntax-tree]]
            [menard.nederlands.basic :as basic]
            [menard.nederlands.complete :as complete]            
            [menard.nederlands.woordenlijst :as woordenlijst]
            [menard.morphology :refer [morph-leaf]]
            [dag_unify.core :as u]
            [clojure.test :refer [deftest is]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

(deftest adjective-agreement
  (is (= "het oude huis"
         (morph (generate {:cat :noun
                           :rule "np:2"
                           :subcat []
                           :root "huis"
                           :agr {:number :sing}
                           :max-depth 2
                           :sem {:quant :the
                                 :pred :house
                                 :mod {:first {:pred :old
                                               :mod []}
                                       :rest []}}}))))

  (if basic/create-model?
    (is (= "het oude huis"
           (morph (generate {:cat :noun
                             :rule "np:2"
                             :subcat []
                             :root "huis"
                             :agr {:number :sing}
                             :max-depth 2
                             :sem {:quant :the
                                   :mod {:first {:pred :old
                                                 :mod []}
                                         :rest []}}}
                            basic/model)))))

  (is (= "een oud huis"
         (morph (generate {:cat :noun
                           :rule "np:2"
                           :subcat []
                           :root "huis"
                           :max-depth 2                           
                           :agr {:number :sing}
                           :sem {:quant :some
                                 :pred :house
                                 :mod {:first {:pred :old
                                               :mod []}
                                       :rest []}}}))))

  (if basic/create-model?
    (is (= "een oud huis"
           (morph (generate {:cat :noun
                             :rule "np:2"
                             :subcat []
                             :max-depth 2                             
                             :root "huis"
                             :agr {:number :sing}
                             :sem {:quant :some
                                   :mod {:first {:pred :old
                                                 :mod []}
                                         :rest []}}}
                            basic/model)))))


  (is (= "de oude huizen"
         (morph (generate {:cat :noun
                           :rule "np:2"
                           :subcat []
                           :max-depth 2                             
                           :root "huis"
                           :agr {:number :plur}
                           :sem {:quant :the
                                 :mod {:first {:pred :old
                                               :number? false
                                               :mod []}
                                       :rest []}}}))))


  (if basic/create-model?
    (is (= "de oude huizen"
           (morph (generate {:cat :noun
                             :rule "np:2"
                             :subcat []
                             :max-depth 2                             
                             :root "huis"
                             :agr {:number :plur}
                             :sem {:quant :the
                                   :mod {:first {:pred :old
                                                 :number? false
                                                 :mod []}
                                         :rest []}}}
                            basic/model))))))

(def generate-per-expression 5)

;; exclude non-production expressions:
(def prod-expressions (->> expressions
                      (filter #(= true (u/get-in % [:prod?] true)))))

(deftest all-expressions-work-1
  (let [start 0
        end (count prod-expressions)
        expression-sets
        (->>
         (range start end)
         (map (fn [index]
                {:i index
                 :expressions (take generate-per-expression
                                    (repeatedly #(generate (nth expressions index))))})))]
    (doall
     (map (fn [expression-set]
            (let [i (:i expression-set)
                  expressions (:expressions expression-set)]
              (log/info (str "testing expression set " i ".."))
              (is (not (contains? (set expressions) nil)))))
          expression-sets))))

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


(def morphology (-> complete/model deref :morphology))

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
   (= "[s(:present-simple){-} .Corona +[vp-slash-object{-} +[modal+subject(:present-simple){-} +moeten .wij] .[adverb-vp{-} .samen +bestrijden]]]"
      (->> "Corona moeten wij samen bestrijden" nl/parse (filter #(= false (u/get-in % [:reflexive?])))  (map nl/syntax-tree) (take 1) first)))
  (is
   (= "[s(:present-simple){-} .ik +[vp-modal-np(:present-simple){-} +probeer .[vp-np(:infinitive){-} .honden +[vp-te{-} +te .zien]]]]"
      (->> "ik probeer honden te zien" nl/parse (filter #(= false (u/get-in % [:reflexive?]))) (map nl/syntax-tree) (take 1) first)))
  (is
   (= "[s(:present-simple){-} .ik +[vp-modal-te(:present-simple){-} +probeer .[vp-te{-} +te .zien]]]"
      (->> "ik probeer te zien" nl/parse (filter #(= false (u/get-in % [:reflexive?]))) (map nl/syntax-tree) (take 1) first))))

;; If true, generates Dutch, then parses it, so we test
;; parsing as well as generation.
(def intermediate-parsing? true)

(defn validator
  "example usage: 
  (validator (nth nl/expressions 15))))))"
  [spec i & [times]]
  (let [times (or times 10)]
    (count (take
            times
            (repeatedly
             #(binding [menard.generate/log-these-rules #{
                                                          ;; add rules
                                                          ;; you want to debug here:
                                                          }
                        menard.generate/log-all-rules? false]
                (-> spec
                    ((fn [spec]
                       (log/info (str "Generating with spec: #" i))
                       spec))
                    nl/generate
                    ((fn [expression]
                       (if (nil? expression)
                         (menard.exception/exception (str "failed to generate using spec: " spec))
                       expression)))
                    ((fn [expression]
                       (if intermediate-parsing?
                         (let [first-result (-> expression nl/morph nl/parse first)]
                           (is (nil? (:menard.parse/partial? first-result)))
                           (log/info (str "Successfully parsed generated expression: " (-> expression nl/syntax-tree)))
                           first-result)
                         expression)))
                    nl/morph
                    println)))))))

(deftest validations
  (let [start 0
        end (count nl/expressions)
        do-times 20]
  (doall
   (->>
    (range start end)
    (map (fn [x]
           (log/info (str "doing " do-times " validation(s) for expression number " x))
           (is (= do-times (menard.test.nederlands/validator (nth nl/expressions x) x do-times)))))))))

(deftest separable-verbs
  (let [treden
        (->> "treden"
             analyze
             (filter #(= :verb (u/get-in % [:cat])))
             (filter #(= "optreden" (u/get-in % [:canonical]))))]
    (is (seq treden))
    (is (= :plur
           (-> treden
               first
               (u/get-in [:subcat :1 :agr :number])))))
  (let [treedt-2nd-sing
        (->> "treedt"
             analyze
             (filter #(= :verb (u/get-in % [:cat])))
             (filter #(= "optreden" (u/get-in % [:canonical])))
             (filter #(= :2nd (u/get-in % [:subcat :1 :agr :person])))
             (filter #(= :sing (u/get-in % [:subcat :1 :agr :number]))))]
    (is (seq treedt-2nd-sing))))


(deftest woordenlijst
  (let [spec {:rule "np:2" :cat :noun :max-depth 1 :phrasal? true :subcat []}]
    (is (not (empty?
              (->
               spec
               (generate woordenlijst/model)
               ((fn [x] (str (morph x) ":" (dag_unify.diagnostics/strip-refs (u/get-in x [:sem :pred])))))))))))


