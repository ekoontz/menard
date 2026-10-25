(ns menard.test.english.direct-objects
  (:require [menard.english :as en :refer [generate morph parse]]
            [dag_unify.core :as u]
            [clojure.test :refer [deftest is testing]]
            #?(:clj [clojure.tools.logging :as log])
            #?(:cljs [cljslog.core :as log])))

(defn object-preds
  "the :sem :obj :pred of each parse of _expression_."
  [expression]
  (set (map #(u/get-in % [:sem :obj :pred]) (parse expression))))

(defn generation-spec
  "the parts of a parse that generate needs in order to rebuild it."
  [parsed]
  {:phrasal? (u/get-in parsed [:phrasal?])
   :cat (u/get-in parsed [:cat])
   :subcat (u/get-in parsed [:subcat])
   :reflexive? (u/get-in parsed [:reflexive?])
   :sem (u/get-in parsed [:sem])})

(deftest parse-direct-object-pronouns
  (testing "a pronoun in the direct object position is the object of the verb"
    (doseq [[expression pred] [["she sees him" :he]
                               ["he sees her" :she]
                               ["they see us" :we]
                               ["I see them" :they]
                               ["I see it" :it]
                               ["I see you" :you]]]
      (is (seq (parse expression)) expression)
      (is (contains? (object-preds expression) pred)
          (str expression " should have a direct object with :pred " pred))))
  (testing "reflexive direct object pronouns"
    (doseq [expression ["I see myself"
                        "you see yourself"
                        "he sees himself"
                        "she sees herself"
                        "it sees itself"
                        "we see ourselves"
                        "they see themselves"]]
      (is (seq (parse expression)) expression))))

(deftest generate-direct-object-pronouns
  (testing "a parsed direct object pronoun can be generated again"
    (doseq [expression ["she sees him"
                        "he sees her"
                        "I see it"]]
      (is (some #{expression}
                (->> expression
                     parse
                     (map generation-spec)
                     (map generate)
                     (map morph)))
          expression))))
