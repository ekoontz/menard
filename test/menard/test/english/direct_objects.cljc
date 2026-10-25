(ns menard.test.english.direct-objects
  (:require [menard.english :as en :refer [generate morph parse syntax-tree]]
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

(defn human-object?
  "True if any parse of _expression_ has a human direct object. English spells
   the human and the nonhuman object pronouns alike ('them'), so the object's
   :ref :human? is what tells them apart."
  [expression]
  (boolean (some #(true? (u/get-in % [:sem :obj :ref :human?])) (parse expression))))

(deftest play-takes-an-inanimate-object
  (testing "play takes a direct object, but not a human one"
    (is (contains? (object-preds "I play it") :it))
    (is (contains? (object-preds "I play a book") :book)
        "an inanimate noun phrase is still an object")
    (doseq [expression ["I play him" "I play her" "I play them"]]
      (is (not (human-object? expression)) expression)))
  (testing "the human object pronouns still work with other verbs"
    (is (human-object? "I see him"))
    (is (human-object? "I see them"))))

(deftest eat-and-think-take-an-inanimate-object
  (testing "eat and think take a direct object, but not a human one"
    (doseq [expression ["I eat him" "I eat her" "I eat them"
                        "I think him" "I think her" "I think them"]]
      (is (not (human-object? expression)) expression)))
  (testing "an inanimate object is kept"
    (is (some #(map? (u/get-in % [:sem :obj])) (parse "I eat a book")))
    (is (some #(map? (u/get-in % [:sem :obj])) (parse "I think it"))))
  (testing "the object-free reading is kept"
    (doseq [expression ["I eat" "I think"]]
      (is (some #(re-find #"\[s" (str (en/syntax-tree %))) (parse expression))
          expression))))

(deftest do-and-make-take-an-inanimate-object
  (testing "do and make take a direct object, but not a human one"
    (doseq [expression ["I do him" "I do her" "I do them"
                        "I make him" "I make her" "I make them"]]
      (is (not (human-object? expression)) expression)))
  (testing "an inanimate object is kept"
    (is (some #(map? (u/get-in % [:sem :obj])) (parse "I do it")))
    (is (some #(map? (u/get-in % [:sem :obj])) (parse "I make a book"))))
  (testing "the object-free reading is kept"
    (doseq [expression ["I do" "I make"]]
      (is (some #(re-find #"\[s" (str (en/syntax-tree %))) (parse expression))
          expression))))

(deftest open-takes-an-inanimate-object
  (testing "open takes a direct object, but not a human one"
    (doseq [expression ["I open him" "I open her" "I open them"]]
      (is (not (human-object? expression)) expression)))
  (testing "an inanimate object is kept"
    (is (some #(map? (u/get-in % [:sem :obj])) (parse "I open a book")))
    (is (some #(map? (u/get-in % [:sem :obj])) (parse "I open it"))))
  (testing "the object-free reading is kept"
    (is (some #(re-find #"\[s" (str (en/syntax-tree %))) (parse "I open")))))

(deftest have-takes-an-inanimate-object
  (testing "have takes a direct object, but not a human one"
    (doseq [expression ["I have him" "I have her" "I have them"]]
      (is (not (human-object? expression)) expression)))
  (testing "an inanimate object is kept"
    (is (some #(map? (u/get-in % [:sem :obj])) (parse "I have a book")))
    (is (some #(map? (u/get-in % [:sem :obj])) (parse "I have it"))))
  (testing "the object-free reading is kept"
    (is (some #(re-find #"\[s" (str (en/syntax-tree %))) (parse "I have"))))
  (testing "the perfect auxiliary is unaffected"
    (is (seq (parse "I have seen him")))))

(deftest drink-takes-an-inanimate-object
  (testing "drink takes a direct object, but not a human one"
    (doseq [expression ["I drink him" "I drink her" "I drink them"]]
      (is (not (human-object? expression)) expression)))
  (testing "an inanimate object is kept"
    (is (some #(map? (u/get-in % [:sem :obj])) (parse "I drink it"))))
  (testing "the object-free reading is kept"
    (is (some #(re-find #"\[s" (str (en/syntax-tree %))) (parse "I drink")))))

(deftest read-takes-an-inanimate-object
  (testing "read takes a direct object, but not a human one"
    (doseq [expression ["I read him" "I read her" "I read them"]]
      (is (not (human-object? expression)) expression)))
  (testing "an inanimate object is kept"
    (is (some #(map? (u/get-in % [:sem :obj])) (parse "I read it"))))
  (testing "the object-free reading is kept"
    (is (some #(re-find #"\[s" (str (en/syntax-tree %))) (parse "I read")))))

(defn sentence-parses
  "the parses of _expression_ that are whole sentences."
  [expression]
  (filter #(re-find #"^\[s" (str (en/syntax-tree %))) (parse expression)))

(defn ditransitive-parses
  "the sentence parses of _expression_ that have an indirect object phrase."
  [expression]
  (filter #(re-find #"pp:" (str (en/syntax-tree %))) (sentence-parses expression)))

(deftest give-takes-a-thing-and-a-person
  (testing "a nonhuman direct object and a human recipient"
    (doseq [expression ["she gives it to her"
                        "I give the book to her"
                        "I give it to him"]]
      (let [parses (ditransitive-parses expression)]
        (is (seq parses) expression)
        (doseq [p parses]
          (is (not (true? (u/get-in p [:sem :obj :ref :human?]))) expression)
          (is (true? (u/get-in p [:sem :iobj :ref :human?])) expression)))))
  (testing "a human direct object is rejected: the thing given is not a person"
    (is (empty? (ditransitive-parses "I give him to her"))))
  (testing "a nonhuman recipient is rejected"
    (is (empty? (ditransitive-parses "I give it to it"))))
  (testing "the object-free reading is kept"
    (is (seq (sentence-parses "I give")))))

(deftest dance-is-intransitive
  (doseq [expression ["I dance it" "I dance the tango"]]
    (is (not-any? #(map? (u/get-in % [:sem :obj])) (parse expression)) expression)))

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

(deftest attend-and-stay-are-intransitive
  (doseq [expression ["I attend a book" "I stay a book" "I stay him"]]
    (is (not-any? #(map? (u/get-in % [:sem :obj])) (parse expression)) expression))
  (doseq [expression ["I attend" "I stay"]]
    (is (some #(re-find #"\[s" (str (en/syntax-tree %))) (parse expression))
        expression)))
