(ns menard.test.italiano.direct-objects
  (:require [menard.italiano :as it :refer [analyze morph parse syntax-tree]]
            [dag_unify.core :as u]
            [clojure.test :refer [deftest is testing]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

(defn object-preds
  "the :sem :obj :pred of each parse of _expression_."
  [expression]
  (set (map #(u/get-in % [:sem :obj :pred]) (parse expression))))

(deftest parse-direct-object-pronouns
  (testing "a clitic direct object pronoun is the object of the verb"
    (doseq [[expression pred] [["lo vedo" :he]
                               ["la vedo" :she]
                               ["li vedo" :they]
                               ["le vedo" :they]
                               ["mi vede" :i]
                               ["ti vedo" :you]
                               ["ci vede" :we]
                               ["vi vedo" :you]]]
      (is (seq (parse expression)) expression)
      (is (contains? (object-preds expression) pred)
          (str expression " should have a direct object with :pred " pred))))
  (testing "with an explicit subject"
    (doseq [[expression pred] [["io lo vedo" :he]
                               ["io li vedo" :they]
                               ["il cane lo vede" :he]]]
      (is (seq (parse expression)) expression)
      (is (contains? (object-preds expression) pred)
          (str expression " should have a direct object with :pred " pred))))
  (testing "a nominative pronoun is not used as a clitic direct object"
    (is (empty? (parse "io vedo lui"))))
  (testing "a full noun phrase direct object lands in :sem :obj too"
    (is (contains? (set (map #(u/get-in % [:sem :obj :pred]) (parse "il cane vede il gatto")))
                   :cat))))

(deftest play-takes-an-inanimate-object
  (testing "play takes a clitic direct object, but not a human one"
    (doseq [expression ["lo gioco" "li gioco" "lo suono" "le suono"]]
      (is (not-any? #(true? (u/get-in % [:sem :obj :human?])) (parse expression))
          expression)))
  (testing "the inanimate reading of the clitic is kept"
    (is (some #(false? (u/get-in % [:sem :obj :human?])) (parse "lo gioco"))))
  (testing "the human clitics still work with other verbs"
    (is (some #(true? (u/get-in % [:sem :obj :human?])) (parse "lo vedo"))))
  (testing "the intransitive reading is kept"
    (is (seq (parse "gioco")))
    (is (seq (parse "suono")))))

(deftest imparare-takes-an-inanimate-object
  (testing "the human clitic is rejected, the inanimate one is not"
    (doseq [expression ["lo imparo" "la imparo" "li imparo"]]
      (is (not-any? #(true? (u/get-in % [:sem :obj :human?])) (parse expression))
          expression))
    (is (some #(false? (u/get-in % [:sem :obj :human?])) (parse "lo imparo"))))
  (testing "the intransitive reading is kept"
    (is (seq (parse "imparo")))))

(deftest mangiare-and-pensare-take-an-inanimate-object
  (testing "the human clitic is rejected, the inanimate one is not"
    (doseq [expression ["lo mangio" "la mangio" "li mangio"
                        "lo penso" "la penso" "li penso"]]
      (is (not-any? #(true? (u/get-in % [:sem :obj :human?])) (parse expression))
          expression))
    (doseq [expression ["lo mangio" "lo penso"]]
      (is (some #(false? (u/get-in % [:sem :obj :human?])) (parse expression))
          expression)))
  (testing "an edible noun phrase is still an object"
    (is (some #(= :cipolla (u/get-in % [:sem :obj :pred])) (parse "mangio la cipolla"))))
  (testing "the intransitive reading is kept"
    (doseq [expression ["mangio" "penso"]]
      (is (seq (parse expression)) expression))))

(deftest fare-takes-an-inanimate-object
  (testing "do and make take a clitic direct object, but not a human one"
    (doseq [expression ["lo faccio" "la faccio" "li faccio"]]
      (is (not-any? #(true? (u/get-in % [:sem :obj :human?])) (parse expression))
          expression))
    (is (some #(false? (u/get-in % [:sem :obj :human?])) (parse "lo faccio"))))
  (testing "the intransitive reading is kept"
    (is (seq (parse "faccio")))))

(deftest aprire-takes-an-inanimate-object
  (testing "open takes a clitic direct object, but not a human one"
    (doseq [expression ["lo apro" "la apro" "li apro"]]
      (is (not-any? #(true? (u/get-in % [:sem :obj :human?])) (parse expression))
          expression))
    (is (some #(false? (u/get-in % [:sem :obj :human?])) (parse "lo apro"))))
  (testing "the intransitive reading is kept"
    (is (seq (parse "apro")))))

(deftest avere-takes-an-inanimate-object
  (testing "have takes a clitic direct object, but not a human one"
    (doseq [expression ["lo ho" "la ho" "li ho"]]
      (is (not-any? #(true? (u/get-in % [:sem :obj :human?])) (parse expression))
          expression))
    (is (some #(false? (u/get-in % [:sem :obj :human?])) (parse "lo ho"))))
  (testing "the intransitive reading is kept"
    (is (seq (parse "ho"))))
  (testing "avere is still the perfect auxiliary"
    (is (seq (parse "ho mangiato")))))

(deftest bere-takes-an-inanimate-object
  (testing "drink takes a clitic direct object, but not a human one"
    (doseq [expression ["lo bevo" "la bevo" "li bevo"]]
      (is (not-any? #(true? (u/get-in % [:sem :obj :human?])) (parse expression))
          expression))
    (is (some #(false? (u/get-in % [:sem :obj :human?])) (parse "lo bevo"))))
  (testing "a drinkable noun phrase is still an object"
    (is (some #(= :birra (u/get-in % [:sem :obj :pred])) (parse "bevo la birra"))))
  (testing "the intransitive reading is kept"
    (is (seq (parse "bevo")))))

(deftest leggere-takes-an-inanimate-object
  (testing "read takes a clitic direct object, but not a human one"
    (doseq [expression ["lo leggo" "la leggo" "li leggo"]]
      (is (not-any? #(true? (u/get-in % [:sem :obj :human?])) (parse expression))
          expression))
    (is (some #(false? (u/get-in % [:sem :obj :human?])) (parse "lo leggo"))))
  (testing "a noun phrase is still an object"
    (is (some #(= :book (u/get-in % [:sem :obj :pred])) (parse "leggo il libro"))))
  (testing "the intransitive reading is kept"
    (is (seq (parse "leggo")))))

(deftest cancellare-riparare-and-parlare-take-an-inanimate-object
  (testing "the human clitic is rejected, the inanimate one is not"
    (doseq [expression ["lo cancello" "la cancello" "lo riparo" "la riparo"
                        "lo parlo" "la parlo"]]
      (is (not-any? #(true? (u/get-in % [:sem :obj :human?])) (parse expression))
          expression))
    (doseq [expression ["lo cancello" "lo riparo" "lo parlo"]]
      (is (some #(false? (u/get-in % [:sem :obj :human?])) (parse expression))
          expression)))
  (testing "the intransitive readings are kept"
    (doseq [expression ["cancello" "riparo" "parlo"]]
      (is (seq (parse expression)) expression))))

(deftest pulire-takes-an-inanimate-object
  (testing "the human clitic is rejected, the inanimate one is not"
    (doseq [expression ["lo pulisco" "la pulisco" "li pulisco"]]
      (is (not-any? #(true? (u/get-in % [:sem :obj :human?])) (parse expression))
          expression))
    (is (some #(false? (u/get-in % [:sem :obj :human?])) (parse "lo pulisco"))))
  (testing "the intransitive reading is kept"
    (is (seq (parse "pulisco")))))

(deftest more-verbs-take-an-inanimate-object
  (testing "the human clitic is rejected, the inanimate one is not"
    (doseq [expression ["lo taglio" "la taglio" "lo guido" "la guido"
                        "lo spendo" "lo so" "lo conservo" "lo formo" "lo presto"
                        "lo abbasso" "la abbasso" "lo compro" "lo riempio"
                        "lo spengo" "lo canto"]]
      (is (not-any? #(true? (u/get-in % [:sem :obj :human?])) (parse expression))
          expression))
    (doseq [expression ["lo taglio" "lo guido" "lo spendo" "lo so"
                        "lo conservo" "lo formo" "lo presto" "lo abbasso"
                        "lo compro" "lo riempio" "lo spengo" "lo canto"]]
      (is (some #(false? (u/get-in % [:sem :obj :human?])) (parse expression))
          expression)))
  (testing "the intransitive readings are kept"
    (doseq [expression ["taglio" "guido" "spendo" "so" "conservo" "formo"
                        "presto" "abbasso" "compro" "riempio" "spengo" "canto"]]
      (is (seq (parse expression)) expression))))

(deftest abbracciare-takes-a-human-object
  (testing "only the human reading of the clitic survives"
    (doseq [expression ["lo abbraccio" "la abbraccio" "li abbraccio"]]
      (is (not-any? #(false? (u/get-in % [:sem :obj :human?])) (parse expression))
          expression)
      (is (some #(true? (u/get-in % [:sem :obj :human?])) (parse expression))
          expression))))

(deftest mettere-takes-an-inanimate-object
  (testing "put takes a clitic direct object, but not a human one"
    (doseq [expression ["lo metto" "la metto" "li metto"]]
      (is (not-any? #(true? (u/get-in % [:sem :obj :human?])) (parse expression))
          expression))
    (is (some #(false? (u/get-in % [:sem :obj :human?])) (parse "lo metto"))))
  (testing "the intransitive reading is kept"
    (is (seq (parse "metto")))))

(deftest ballare-is-intransitive
  (doseq [expression ["lo ballo" "li ballo" "la ballo"]]
    (is (empty? (parse expression)) expression)))

(deftest commentare-takes-a-direct-object
  (testing "commentare is not object-less, so its clitics work again"
    (doseq [expression ["lo commento" "la commento" "li commento"]]
      (is (seq (parse expression)) expression))
    (is (some #(true? (u/get-in % [:sem :obj :human?])) (parse "lo commento")))
    (is (some #(false? (u/get-in % [:sem :obj :human?])) (parse "lo commento"))))
  (testing "a noun phrase is an object too"
    (is (seq (parse "commento il libro")))))

(deftest these-verbs-are-intransitive
  (doseq [expression ["lo sto" "la sto" "lo frequento" "li frequento"
                      "lo sembro" "la sembro" "li sembro"
                      "lo ceno" "la ceno"
                      "lo vengo" "lo entro"
                      "lo vivo" "lo cado" "lo nasco" "lo chiacchiero"]]
    (is (empty? (parse expression)) expression))
  (testing "the intransitive readings are kept"
    (doseq [expression ["sto" "io sto" "frequento" "sembro" "sembra"
                        "ceno" "io ceno" "vengo" "io vengo" "commento"
                        "entro" "io entro"
                        "riposo" "vivo" "cado" "nasco"
                        "chiacchiero" "io chiacchiero"]]
      (is (seq (parse expression)) expression)))
  (testing "riposare's own (non-reflexive) reading takes no object; riposarsi
            still does, since it is the reflexive verb"
    (is (not-any? #(and (= :rest (u/get-in % [:sem :pred]))
                         (some? (u/get-in % [:sem :obj :pred])))
                  (parse "lo riposo"))))
  (testing "stare is still the progressive auxiliary"
    (is (seq (parse "sto camminando")))))
