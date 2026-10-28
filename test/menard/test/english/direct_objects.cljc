(ns menard.test.english.direct-objects
  (:require [menard.english :as en :refer [generate morph parse syntax-tree]]
            [menard.morphology :refer [morph-leaf]]
            [menard.english.complete :as en-complete]
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

(deftest eat-takes-an-inanimate-object
  (testing "eat takes a direct object, but not a human one"
    (doseq [expression ["I eat him" "I eat her" "I eat them"]]
      (is (not (human-object? expression)) expression)))
  (testing "an inanimate object is kept"
    (is (some #(map? (u/get-in % [:sem :obj])) (parse "I eat a book"))))
  (testing "the object-free reading is kept"
    (doseq [expression ["I eat"]]
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

(deftest learn-and-practice-take-an-inanimate-object
  (testing "learn and practice take a direct object, but not a human one"
    (doseq [expression ["I learn him" "I learn her" "I learn them"
                        "I practice him" "I practice her" "I practice them"]]
      (is (not (human-object? expression)) expression)))
  (testing "an inanimate object is kept"
    (is (= #{:it} (object-preds "I learn it")))
    (is (= #{:book} (object-preds "I learn a book")))
    (is (= #{:it} (object-preds "I practice it"))))
  (testing "the object-free reading is kept"
    (doseq [expression ["I learn" "I practice"]]
      (is (some #(re-find #"\[s" (str (en/syntax-tree %))) (parse expression))
          expression))))

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

(deftest put-takes-a-thing-and-a-place
  (testing "the thing put and the place it goes are both nonhuman"
    (doseq [expression ["I put the book on the table"
                        "I put it on the table"
                        "I put it in the house"]]
      (let [parses (ditransitive-parses expression)]
        (is (seq parses) expression)
        (doseq [p parses]
          (is (not (true? (u/get-in p [:sem :obj :ref :human?]))) expression)
          (is (not (true? (u/get-in p [:sem :iobj :ref :human?]))) expression)))))
  (testing "a human thing put is rejected"
    (is (empty? (ditransitive-parses "I put him on the table"))))
  (testing "a human place is rejected"
    (doseq [expression ["I put it on her" "I put it in her"]]
      (is (empty? (ditransitive-parses expression)) expression)))
  (testing "a plain direct object is allowed: the thing put, no place given"
    (doseq [expression ["I put them" "I put it"]]
      (is (some #(some? (u/get-in % [:sem :obj :pred])) (sentence-parses expression))
          expression))
    (is (not-any? #(some? (u/get-in % [:sem :obj :pred])) (sentence-parses "I put him"))))
  (testing "the object-free reading is kept"
    (is (seq (sentence-parses "I put")))))

(deftest long-vowel-stems-do-not-double-their-final-consonant
  (testing "seem/seemed, not seemmed (the doubling rule needs a single vowel)"
    (doseq [infl [:past-simple :past-participle]]
      (is (= "seemed" (morph-leaf {:canonical "seem" :cat :verb :infl infl} (:morphology @en-complete/model)))
          (str infl))))
  (testing "a single vowel in a stressed final syllable still doubles"
    (doseq [[canonical expected] [["stop" "stopped"] ["step" "stepped"]
                                  ["fit" "fitted"] ["plan" "planned"]
                                  ["chat" "chatted"]]]
      (is (= expected (morph-leaf {:canonical canonical :cat :verb :infl :past-simple} (:morphology @en-complete/model)))
          canonical)
      (is (= expected (morph-leaf {:canonical canonical :cat :verb :infl :past-participle} (:morphology @en-complete/model)))
          (str canonical " (participle)"))))
  (testing "a long vowel does not double, and it is spelled \"eat\" not \"at\""
    (doseq [[canonical expected] [["cheat" "cheated"] ["repeat" "repeated"]
                                  ["open" "opened"] ["explain" "explained"]]]
      (is (= expected (morph-leaf {:canonical canonical :cat :verb :infl :past-simple} (:morphology @en-complete/model)))
          canonical))))

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

(deftest comment-is-transitive-only
  (testing "comment needs a direct object"
    (is (empty? (sentence-parses "I comment")))
    (doseq [expression ["I comment it" "I comment a book" "I comment him"]]
      (is (seq (sentence-parses expression)) expression)
      (is (some #(some? (u/get-in % [:sem :obj :pred])) (sentence-parses expression))
          expression)))
  (testing "\"comment on\" is its own prepositional sense"
    (is (seq (sentence-parses "I comment on it")))))

(deftest enter-takes-an-inanimate-object
  (testing "enter takes an object, but not a human one"
    (doseq [expression ["I enter him" "I enter her" "I enter them"]]
      (is (not (human-object? expression)) expression)))
  (testing "an inanimate object is kept"
    (is (= #{:it} (object-preds "I enter it")))
    (is (= #{:book} (object-preds "I enter a book"))))
  (testing "the object-free reading is kept"
    (is (some #(re-find #"\[s" (str (en/syntax-tree %))) (parse "I enter")))))

(deftest work-is-intransitive
  (testing "work takes no direct object, human or not"
    (doseq [expression ["I work a book" "I work it" "I work him"]]
      (is (not-any? #(map? (u/get-in % [:sem :obj])) (parse expression))
          expression)))
  (testing "the object-free reading is kept, for both of its senses"
    (is (some #(re-find #"\[s" (str (en/syntax-tree %))) (parse "I work")))
    (is (some #(re-find #"\[s" (str (en/syntax-tree %))) (parse "they work")))))

(deftest think-is-intransitive
  (testing "thinking takes no noun object, human or inanimate"
    ;; the parser also hands back parses that ignore trailing words ("you were
    ;; thinking them" parses without "them"), so what matters is that no parse
    ;; makes the pronoun the verb's object.
    (doseq [expression ["I think him" "I think her" "I think them"
                        "I think it" "I think a book" "you were thinking them"]]
      (is (not-any? #(some? (u/get-in % [:sem :obj :pred])) (parse expression))
          expression)))
  (testing "the object-free reading is kept"
    (is (some #(re-find #"\[s" (str (en/syntax-tree %))) (parse "I think"))))
  (testing "the clausal senses still take their complement"
    (is (seq (sentence-parses "I think that I see the cat")))))
