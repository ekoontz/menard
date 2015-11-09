(ns babel.english.morphology
  (:refer-clojure :exclude [get-in merge resolve replace reverse]))

(require '[babel.pos :refer (noun)])
(require '[clojure.core :as core])
(require '[clojure.string :refer :all])
(require '[clojure.string :as string])
(require '[clojure.tools.logging :as log])
(require '[dag-unify.core :refer :all])

(declare get-string-1)
(declare plural-en)

(defn fo [input]
  (cond 

   (= input :fail)
   (str input)

   (= (type input) clojure.lang.LazySeq)
   (str "['" (string/join "','" (map fo input)) "']")

   (string? input)
   input

   (and (map? input)
        (get-in input [:english :a :english])
        (get-in input [:english :b :english])
        (or (map? (get-in input [:english :a :english]))
            (map? (get-in input [:english :b :english]))))
   (string/join " "
                (list (get-string-1 (get-in input [:english :a :english]))
                      (get-string-1 (get-in input [:english :b :english]))))
   (and (map? input)
        (get-in input [:english :a])
        (get-in input [:english :b]))
   (string/join " "
                (list (get-string-1 (get-in input [:english :a]))
                      (get-string-1 (get-in input [:english :b]))))

   (and (map? input)
        (map? (get-in input [:english])))
   (get-string-1 (get-in input [:english]))

   (and (map? input)
        (get-in input [:english]))
   (get-string-1 input)

   (and (map? input)
        (get-in input [:a])
        (get-in input [:b]))
   (str (string/join " " 
                     (list (fo (get-in input [:a]))
                           (fo (get-in input [:b])))))
   (or (seq? input)
       (vector? input))
   (str "(" (string/join " , " 
                         (remove #(= % "")
                                 (map #(let [f (fo %)] (if (= f "") "" (str "" f ""))) input)))
        ")")

   true
   ""))

(defn get-string-1 [word]
  (log/debug (str "get-string-1: " word))
  (cond
   (ref? word)
   (get-string-1 @word)

   (= word :top)
   ".."

   (and (map? word)
        (nil? (:a word))
        (nil? (:b word))
        (nil? (:english word))
        (nil? (:english word)))
   ".."

   ;; "to do [past]" + "well" => "did well"
   (and (= (get-in word '(:cat)) :verb)
        (= (get-in word '(:infl)) :past)
        (string? (get-in word '(:a :past))))
   (str (get-in word '(:a :past)) " "
        (get-string-1 (get-in word '(:b))))

   ;; :note is used for little annotations that are significant in italian but not in english
   ;; e.g. gender signs (♂,♀) on nouns like "professore" and "professoressa".
   (and (string? (get-in word '(:english)))
        (string? (get-in word '(:note))))
   (str (trim (get-string-1 (dissoc word :note))) " (" (trim (get-in word '(:note))) ")")

   (= (get-in word '(:a)) :top)
   (str
    ".." " " (get-string-1 (get-in word '(:b))))

   ;; show elipsis (..) if :b is not specified.
   (and
    (= (get-in word '(:b)) :top)
    (string? (get-string-1 (get-in word '(:a)))))
   (str
    (get-string-1 (get-in word '(:a)))
    " " "..")

   ;; show elipsis (..) if :a is not specified.
   (and
    (= (get-in word '(:b)) :top)
    (string? (get-in word '(:a :english))))
   (str
    (get-string-1 (get-in word '(:a :english)))
    " " "..")

   (string? word)
   (trim word)

   ;; (could have) + (to go) => "could have gone"
   (and
    (get-in word '(:a))
    (get-in word '(:b))
    (string? (get-in word '(:a :past)))
    (= (get-in word '(:a :past)) "could have")
    (string? (get-in word '(:b :past-participle)))
    (= (get-in word '(:a :infl)) :past))
   (join " " (list (get-in word '(:a :past))
                          (get-in word '(:b :past-participle))))

   ;; (could have) + (to sleep) => "could have slept"
   (and
    (get-in word '(:a))
    (get-in word '(:b))
    (string? (get-in word '(:a :past)))
    (= (get-in word '(:a :past)) "could have")
    (string? (get-in word '(:b :past)))
    (= (get-in word '(:a :infl)) :past))
   (join " " (list (get-in word '(:a :past))
                          (get-in word '(:b :past))))

   ;; (could have) + (do X) => "could have done X"
   (and
    (get-in word '(:a))
    (get-in word '(:b))
    (string? (get-in word '(:a :past)))
    (= (get-in word '(:a :past)) "could have")
    (string? (get-in word '(:b :a :past-participle)))
    (= (get-in word '(:a :infl)) :past))
   ;; recursive call after inflecting '(:b :a) to past.
   (get-string-1 {:a (get-in word '(:a))
                  :b {:a (get-in word '(:b :a :past-participle))
                      :b (get-in word '(:b :b))}})

   ;; (could have) + (make X) => "could have made X"
   (and
    (get-in word '(:a))
    (get-in word '(:b))
    (string? (get-in word '(:a :past)))
    (= (get-in word '(:a :past)) "could have")
    (string? (get-in word '(:b :a :past)))
    (= (get-in word '(:a :infl)) :past))
   ;; recursive call after inflecting '(:b :a) to past.
   (get-string-1 {:a (get-in word '(:a))
                  :b {:a (get-in word '(:b :a :past))
                      :b (get-in word '(:b :b))}})
   (and
    (get-in word '(:a))
    (get-in word '(:b))
    (string? (get-in word '(:a)))
    (string? (get-in word '(:b))))
   (join " "
         (list (get-in word '(:a))
               (get-in word '(:b))))

   (and
    (get-in word '(:a))
    (get-in word '(:b)))
   (join " "
         (list (fo (get-in word '(:a)))
               (fo (get-in word '(:b)))))

   ;; TODO: this seems wrong: how could :infl == :english?
   (and (= :english (get-in word '(:infl)))
        (string? (get-in word '(:english))))
   (get-in word '(:english))

   (= true (get-in word '(:hidden)))
;;   "Ø"
   ""
   (and
    (= true (get-in word '(:a :hidden)))
    (= true (get-in word '(:b :hidden))))
;;   "Ø"
   ""
   (= true (get-in word '(:a :hidden)))
   (get-string-1 (get-in word '(:b)))

   (= true (get-in word '(:b :hidden)))
   (get-string-1 (get-in word '(:a)))

   (and (= (get-in word '(:infl)) :conditional)
        (get-in word '(:english))
        (not (nil? (get-in word '(:agr :number))))
        (not (nil? (get-in word '(:agr :person))))
        (string? (get-in word [:conditional])))
   (str "would " (get-in word [:conditional]))

   (and (= (get-in word '(:infl)) :conditional)
        (get-in word '(:english))
        (not (nil? (get-in word '(:agr :number))))
        (not (nil? (get-in word '(:agr :person)))))
   (let [infinitive (get-in word '(:english))
         stem (replace infinitive #"^to " "")]
     (str "would " stem))

   (and (= (get-in word '(:infl)) :future)
        (get-in word '(:english))
        (not (nil? (get-in word '(:agr :number))))
        (not (nil? (get-in word '(:agr :person))))
        (string? (get-in word [:future])))
   (str "will " (get-in word [:future]))

   (and (= (get-in word '(:infl)) :future)
        (get-in word '(:english))
        (not (nil? (get-in word '(:agr :number))))
        (not (nil? (get-in word '(:agr :person)))))
   (let [infinitive (get-in word '(:english))
         stem (replace infinitive #"^to " "")]
     (str "will " stem))

   (and (= (get-in word '(:infl)) :imperfect)
        (get-in word '(:english)))
   (let [infinitive (get-in word '(:english))
         stem (replace infinitive #"^to " "")
         to-final (re-find #" to$" stem) ;; occurs in e.g. "have to": in imperfect becomes "was having to"
         stem (replace stem #" to$" "")
         stem-minus-one (nth (re-find #"(.*).$" stem) 1)
         penultimate-stem-char (nth (re-find #"(.).$" stem) 1)
         penultimate-stem-char-is-vowel (or (= penultimate-stem-char "a")
                                            (= penultimate-stem-char "e")
                                            (= penultimate-stem-char "i")
                                            (= penultimate-stem-char "o")
                                            (= penultimate-stem-char "u"))
         last-stem-char (re-find #".$" stem)
         last-stem-char-is-e (re-find #"e$" stem)]
     ;; remove final "e", if any, before adding "e": e.g. "write" => "writing"
     (let [stem (if (and last-stem-char-is-e
                         (> (.length stem) 2) ;; don't apply this to "be".
                         (not penultimate-stem-char-is-vowel))
                  stem-minus-one
                  stem)


           ;; unless overridden by :participle or :participle-suffix below,
           ;; ing-form will be used.
           ing-form 
           (cond

            (re-find #"ie$" stem)
            (str (replace stem #"..$" "y") "ing")

            true
            (str stem "ing"))

           ]
       (cond

        ;; TODO: add support for per-agreement (by number or person) irregular participle;
        ;; for now, only support for a single participle irregular form for all agreements.
        ;; (might not be needed for english)

        ;; 2) use irregular that is the same for all number and person if there is one.
        (and (string? (get-in word '(:participle)))
             (= :sing (get-in word '(:agr :number)))
             (or (= :1st (get-in word '(:agr :person)))
                 (= :3rd (get-in word '(:agr :person))))
             (string? (get-in word '(:participle))))
        (str "was " (get-in word '(:participle)))

        (string? (get-in word '(:participle)))
        (str "were " (get-in word '(:participle)))

        (and (= :sing (get-in word '(:agr :number)))
             (or (= :1st (get-in word '(:agr :person)))
                 (= :3rd (get-in word '(:agr :person))))
             (string? (get-in word '(:participle-suffix))))
        (str "was " (get-in word '(:participle-suffix)))

        (string? (get-in word '(:participle-suffix)))
        (str "were " (get-in word '(:participle-suffix)))

        (and (= :sing (get-in word '(:agr :number)))
             (or (= :1st (get-in word '(:agr :person)))
                 (= :3rd (get-in word '(:agr :person)))))
        (str "was " ing-form (if to-final to-final ""))

        true
        (str "were " ing-form (if to-final to-final "")))))

   ;; irregular past (1): a single inflection for all persons/numbers.
   (and (= :past (get-in word '(:infl)))
        (string? (get-in word '(:past))))
   (get-in word '(:past))

   (and (= :past (get-in word '(:infl)))
        (= :top (get-in word '(:agr :number)))
        (string? (get-in word '(:past :2sing))))
   ;; use the 2nd singular form if there's not enough inflection info to go on.
   (str "[" (get-in word '(:past :2sing)) "]")

   (= :top (get-in word '(:infl)))
   (get-in word '(:english))

   ;; irregular past (2): a different inflection for each persons/numbers.
   (and (= :past (get-in word '(:infl)))
        (map? (get-in word '(:past))))
   (let [number (get-in word '(:agr :number))
         person (get-in word '(:agr :person))]
     (cond (and (= person :1st) (= number :sing)
                (string? (get-in word '(:past :1sing))))
           (get-in word '[:past :1sing])

           (and (= person :2nd) (= number :sing)
                (string? (get-in word [:past :2sing])))
           (get-in word '[:past :2sing])

           (and (= person :3rd) (= number :sing)
                (string? (get-in word [:past :3sing])))
           (get-in word '(:past :3sing))

           (and (= person :1st) (= number :plur)
                (string? (get-in word [:past :1plur])))
           (get-in word '(:past :1plur))

           (and (= person :2nd) (= number :plur)
                (string? (get-in word [:past :2plur])))
           (get-in word '(:past :2plur))

           (and (= person :3rd) (= number :plur)
                (string? (get-in word [:past :3plur])))
           (get-in word '(:past :3plur))

           ;; default
           (string? (get-in word [:past :english]))
           (get-in word [:past :english])


           true word)) ;; not enough agreement specified to conjugate.

   ;; regular past
   (and (= :past (get-in word '(:infl)))
        (string? (get-in word '(:english))))
   (let [infinitive (get-in word '(:english))
         stem (replace infinitive #"^to " "")
         stem-minus-one (nth (re-find #"(.*).$" stem) 1)
         penultimate-stem-char (nth (re-find #"(.).$" stem) 1)
         last-stem-char (re-find #".$" stem)
         last-stem-char-is-e (re-find #"e$" stem)
         last-stem-char-is-y (re-find #"y$" stem)]
     (cond last-stem-char-is-e
           (str stem-minus-one "ed")   ;; "bake"->"baked"

           (and last-stem-char-is-y
                (or (= penultimate-stem-char "l")
                    (= penultimate-stem-char "n")
                    (= penultimate-stem-char "r")))
           (str stem-minus-one "ied")  ;; "try"->"tried"

           true
           (str stem "ed"))) ;; "play"->"played"
   (and
    (= :present (get-in word '(:infl)))
    (string? (get-in word '(:english))))
   (let [root (get-in word '(:english))
         ;; TODO: throw exception rather than encoding error "(no root)" as part
         ;; of the english string.
         root (if (nil? root) "(no root)" root)
         root (if (not (= (type root) java.lang.String))
                (get-in word '(:english :english))
                root)
         person (get-in word '(:agr :person))
         number (get-in word '(:agr :number))
         stem (replace root #"^to " "")
         last-stem-char-is-e (re-find #"e$" stem)
         penultimate-stem-char-is-vowel (re-find #"[aeiou].$" stem)
         last-stem-char-is-vowel (re-find #"[aeiou]$" stem)]
     (log/debug "+else")
     (log/debug (str "(english):word: " word))
     (cond

      (and (= person :1st) (= number :sing)
           (string? (get-in word '(:present :1sing))))
      (get-in word '(:present :1sing))

      (and (= person :2nd) (= number :sing)
           (string? (get-in word '(:present :2sing))))
      (get-in word '(:present :2sing))

      (and (= person :3rd) (= number :sing)
           (string? (get-in word '(:present :3sing))))
      (get-in word '(:present :3sing))

      (and (= person :1st) (= number :plur)
           (string? (get-in word '(:present :1plur))))
      (get-in word '(:present :1plur))
      (and (= person :2nd) (= number :plur)
           (string? (get-in word '(:present :2plur))))
      (get-in word '(:present :2plur))
      (and (= person :3rd) (= number :plur)
           (string? (get-in word '(:present :3plur))))
      (get-in word '(:present :3plur))

      (and (= person :1st) (= number :sing))
      (str stem "")

      (and (= person :2nd) (= number :sing))
      (str stem "")

      (and (= person :3rd) (= number :sing)
           (re-find #"[dlnr]y$" stem))
      (let [stem-minus-final-y (replace root #"y$" "")]
        (str stem-minus-final-y "ies"))

      (and (= person :3rd) (= number :sing)
           (re-find #"[cs][sh]$" stem))
      (str stem "es")

      (and (= person :3rd) (= number :sing)
           (re-find #"o$" stem))
      (str stem "es")

      ;; default 3rd sing inflection: just add s.
      (and (= person :3rd) (= number :sing))
      (str stem "s")

      (and (= person :1st) (= number :plur))
      (str stem "")

      (and (= person :2nd) (= number :plur))
      (str stem "")

      (and (= person :3rd) (= number :plur))
      (str stem "")

      (string? (get-in word '(:english)))
      (get-in word '(:english))

      (string? (get-in word '(:english)))
      (get-in word '(:english))

      :else (str root )))

   (and
    (get-in word '(:plur))
    (= (get-in word '(:agr :number)) :plur)
    (= (get-in word '(:cat) :noun)))
   (get-in word '(:plur))

   ;; TODO: remove support for deprecated :root - use :plur instead (as immediately above).
   (and
    (get-in word '(:root :plur))
    (= (get-in word '(:agr :number)) :plur)
    (= (get-in word '(:cat) :noun)))
   (get-in word '(:root :plur))

   ;; TODO: remove support for deprecated :root - use :sing instead.
   (and
    (= (get-in word '(:agr :number)) :sing)
    (= (get-in word '(:cat) :noun))
    (string? (get-in word '(:root))))
   (get-in word '(:root))

   (and
    (= (get-in word '(:agr :number)) :sing)
    (= (get-in word '(:cat) :noun))
    (string? (get-in word '(:english))))
   (trim (str (get-in word '(:english)) " "
              (if (get-in word '(:note))
                (get-in word '(:note))
                "")))

   ;; TODO: remove support for deprecated :root - use :sing instead.
   (and
    (= (get-in word '(:agr :number)) :sing)
    (= (get-in word '(:cat) :noun))
    (string? (get-in word '(:root :english))))
   (get-in word '(:root :english))

   (and
    (= (get-in word '(:agr :number)) :sing)
    (= (get-in word '(:cat) :noun))
    (string? (get-in word '(:english :english))))
   (str
    (trim (get-in word '(:english :english))) " "
    (if (get-in word '(:note))
      (trim (get-in word '(:note)))))

   ;; TODO: remove support for deprecated :root - use :plur instead.
   (and (= (get-in word '(:agr :number)) :plur)
        (= (get-in word '(:cat)) :noun)
        (string? (get-in word '(:root))))
   (str (get-in word '(:root)) "s")

   (and (= (get-in word '(:agr :number)) :plur)
        (= (get-in word '(:cat)) :noun)
        (string? (get-in word '(:english)))
        (not (= (get-in word [:pronoun]) true)))
   (str (plural-en (get-in word '(:english)))
        (if (get-in word '(:note))
          (str (get-in word '(:note)))))

   (and (= (get-in word '(:agr :number)) :plur)
        (= (get-in word '(:cat)) :noun)
        (string? (get-in word '(:english :english)))
        (not (= (get-in word [:pronoun]) true)))
   (str (plural-en (get-in word '(:english :english)))
        (if (get-in word '(:english :note))
          (str (get-in word '(:english :note)))))

   (and (= (get-in word '(:cat)) :adjective)
        (string? (get-in word '(:english))))
   (get-in word '(:english))

   (string? (get-in word '(:english)))
   (get-in word '(:english))

   ;; TODO: not sure if this code is alive or not: is there ever
   ;; a case of a sign with '(:english :english :english)?
   (and (string? (get-in word '(:english :english)))
        (= (.size (keys word)) 1))
   (get-in word '(:english :english))

   (string? (get-in word '(:english)))
   (get-in word '(:english))

   :else
   word))

(defn remove-to [english-verb-phrase]
  (let [english (get english-verb-phrase :english)]
    (let [regex #"^to[ ]+(.*)"]
      (let [string
            (replace english regex (fn [[_ rest]] (str rest)))]
        (merge
         {:remove-to string}
         english-verb-phrase)))))

(defn plural-en [english]
  (if (re-find #"[t][y]$" english) ;; city => cities
    (replace english #"[y]$" "ies")
    (if (re-find #"[hsx]$" english) ;; brush => brushes
      (str english "es")
      ;; default case.
      (str english "s"))))

(declare fo-ps-en)

(defn fo-ps-en [expr]
  "show the phrase-structure of a phrase structure tree, e.g [hh21 'mangiare (to eat)' [cc10 'il (the)' 'pane(bread)']]"
  ;; [:first = {:head,:comp}] will not yet be found in expr, so this head-first? will always be false.
  (let [head-first? (= :head (get-in expr [:first]))]
    (cond

     (and
      (or (set? expr)
          (seq? expr)
          (vector? expr))
      (empty? expr))
     (str "")

     (and
      (or (set? expr)
          (seq? expr)
          (vector? expr))
      (not (empty? expr)))

     ;; expr is a sequence of some kind. Assume each element is a phrase structure tree and show each.
     (map (fn [each]
            (fo-ps-en each))
          expr)


     (and (map? expr)
          (:italiano expr)
          (:english expr))
     (str "it:" (fo-ps-en (:italiano expr)) ";"
          "en:" (fo-ps-en (:english expr)))

     (and (map? expr)
          (:rule expr)
          (= (get-in expr '(:english :a))
             (get-in expr '(:comp :english))))
     ;; complement first
     (str "[" (:rule expr) " "
          (fo-ps-en (get-in expr '(:comp)))
          " "
          (fo-ps-en (get-in expr '(:head)))
          "]")

     (and (map? expr)
          (:rule expr))
     ;; head first ('else' case of above.)
     (str "[" (:rule expr) " "
          (fo-ps-en (get-in expr '(:head)))
          " "
          (fo-ps-en (get-in expr '(:comp)))
          "]")


     (and (map? expr)
          (:comment expr)
          (= (get-in expr '(:english :a))
             (get-in expr '(:comp :english))))
     ;; complement first
     (str "[" (:comment expr) " "
          (fo-ps-en (get-in expr '(:comp)))
          " "
          (fo-ps-en (get-in expr '(:head)))
          "]")

     (and (map? expr)
          (:comment expr))
     ;; head first ('else' case of above.)
     (str "[" (:comment expr) " "
          (fo-ps-en (get-in expr '(:head)))
          " "
          (fo-ps-en (get-in expr '(:comp)))
          "]")

     (and
      (map? expr)
      (:english expr))
     (str (get-string-1 (get-in expr '(:english))))

     true
     expr)))

(def plural-to-singular-noun
  {#"s$"
   {:replace-with ""
    :unify-with {:synsem {:cat :noun
                          :agr {:number :plur}}}}})

(def infinitive-to-infinitive
  {:identity1
   {:unify-with {:synsem {:cat :verb
                          :infl :infinitive}}}})

(def infinitive-to-1sing-present ;; infinitive "read" -> "i read"
  {:identity2
   {:unify-with {:synsem {:cat :verb
                          :infl :present
                          :subcat {:1 {:agr {:number :sing
                                             :person :1st}}}}}}})

(def infinitive-to-2sing-present ;; infinitive "read" -> "you read"
  {:identity3
   {:unify-with {:synsem {:cat :verb
                          :infl :present
                          :subcat {:1 {:agr {:number :sing
                                             :person :2nd}}}}}}})

(def infinitive-to-3sing-present ;; infinitive "read" -> "she reads"
  {#"s$"
   {:replace-with ""
    :unify-with {:synsem {:cat :verb
                          :infl :present
                          :subcat {:1 {:agr {:number :sing
                                             :person :3rd}}}}}}})


(def infinitive-to-past-1
  {#"d$"
   {:replace-with ""
    :unify-with {:synsem {:cat :verb
                          :infl :past}}}})

(def infinitive-to-past-2
  {#"ed$"
   {:replace-with ""
    :unify-with {:synsem {:cat :verb
                          :infl :past}}}})

(def infinitive-to-1plur-present ;; infinitive "read" -> "we read"
  {:identity4
   {:unify-with {:synsem {:cat :verb
                          :infl :present
                          :subcat {:1 {:agr {:number :plur
                                             :person :1st}}}}}}})

(def infinitive-to-2plur-present ;; infinitive "read" -> "you all read"
  {:identity5
   {:unify-with {:synsem {:cat :verb
                          :infl :present
                          :subcat {:1 {:agr {:number :plur
                                             :person :2nd}}}}}}})

(def infinitive-to-3plur-present ;; infinitive "read" -> "they read"
  {:identity6
   {:replace-with ""
    :unify-with {:synsem {:cat :verb
                          :infl :present
                          :subcat {:1 {:agr {:number :plur
                                             :person :3rd}}}}}}})

(def lexical-noun-to-singular
  {:identity
   {:unify-with {:synsem {:cat :noun
                          :agr {:number :sing}}}}})



(defn analyze [surface-form lookup-fn]
  "return the map incorporating the lexical information about a surface form."
  (let [replace-pairs
        (merge 
         plural-to-singular-noun
         infinitive-to-infinitive
         infinitive-to-1sing-present
         infinitive-to-2sing-present
         infinitive-to-3sing-present

         infinitive-to-1plur-present
         infinitive-to-2plur-present
         infinitive-to-3plur-present

         lexical-noun-to-singular ;; turns :number :top to :number :sing

         infinitive-to-past-1 ;; love => loved
         infinitive-to-past-2 ;; talked => talk

         )
        
        analyzed
        (remove fail?
                (mapcat
                 (fn [key]
                   (if (and (not (keyword? key)) (re-find key surface-form))
                     (let [lexical-form (string/replace surface-form key
                                                        (:replace-with (get replace-pairs key)))
                           looked-up (lookup-fn lexical-form)]
                       (map #(unifyc % (:unify-with (get replace-pairs key)))
                            looked-up))))
                 (keys replace-pairs)))

        analyzed-via-identity
        (remove fail?
                (mapcat
                 (fn [key]
                   (if (keyword? key)
                     (let [lexical-form surface-form
                           looked-up (lookup-fn lexical-form)]
                       (map #(unifyc % (:unify-with (get replace-pairs key)))
                            looked-up))))
                 (keys replace-pairs)))]

    (concat
     analyzed

     ;; also lookup the surface form itself, which
     ;; might be either the canonical form of a word, or an irregular conjugation of a word.
     (if (not (empty? analyzed-via-identity))
       analyzed-via-identity
       (lookup-fn surface-form)))))

(def pronoun-semantic-gender-agreement
  (let [gender (ref :top)]
    {:synsem {:sem {:gender gender}
              :agr {:gender gender}}}))

(defn agreement [lexical-entry]
  (cond
   (= (get-in lexical-entry [:synsem :cat]) :verb)
   (let [cat (ref :top)
         infl (ref :top)]
     (unifyc lexical-entry
             {:english {:cat cat
                        :infl infl}
              :synsem {:cat cat
                       :infl infl}}))

   (and (= (get-in lexical-entry [:synsem :cat]) :noun)
        (= (get-in lexical-entry [:synsem :pronoun]) true))
   (let [agr (ref :top)
         cat (ref :top)
         pronoun (ref :top)]
     (unifyc lexical-entry
             {:english {:agr agr
                        :cat cat
                        :pronoun pronoun}
              :synsem {:agr agr
                       :cat cat
                       :pronoun pronoun}}
             pronoun-semantic-gender-agreement))

   (= (get-in lexical-entry [:synsem :cat]) :noun)
   (let [agr (ref :top)
         cat (ref :top)
         pronoun (ref :top)]
     (unifyc lexical-entry
             {:english {:agr agr
                        :cat cat
                        :pronoun pronoun}
              :synsem {:agr agr
                       :cat cat
                       :pronoun pronoun}}))

   true
   lexical-entry))

(def english-specific-rules
  (list agreement))

(defn exception-generator [lexicon]
  (if (not (empty? lexicon))
    (let [lexeme-kv (first lexicon)
          lexemes (second lexeme-kv)]
      (let [result (mapcat (fn [path-and-merge-fn]
                             (let [path (:path path-and-merge-fn)
                                   merge-fn (:merge-fn path-and-merge-fn)]
                               ;; a lexeme-kv is a pair of a key and value. The key is a string (the word's surface form)
                               ;; and the value is a list of lexemes for that string.
                               (log/debug (str (first lexeme-kv) "looking at path: " path))
                               (mapcat (fn [lexeme]
                                         ;; this is where a unify/dissoc that supported
                                         ;; non-maps like :top and :fail, would be useful:
                                         ;; would not need the (if (not (fail? lexeme)..)) check
                                         ;; to avoid a difficult-to-understand "java.lang.ClassCastException: clojure.lang.Keyword cannot be cast to clojure.lang.IPersistentMap" error.
                                         (let [lexeme (cond (= lexeme :fail)
                                                            :fail
                                                            (= lexeme :top)
                                                            :top
                                                            true
                                                            (dissoc (copy lexeme) :serialized))]
                                           (if (string? (get-in lexeme path :none))
                                             (list {(get-in lexeme path)
                                                    (merge
                                                     lexeme
                                                     (unifyc (merge-fn lexeme)
                                                             {:english {:exception true}}))}))))
                                       lexemes)))
                           [
                            ;; 1. plural exceptions: e.g. "men","women":
                            {:path [:english :plur]
                             :merge-fn
                             (fn [val]
                               {:synsem {:cat :noun}
                                :english {:agr {:number :plur}
                                          :english (get-in val [:english :plur])}})}

                            ;; <2. past exceptions: e.g. "sleep" -> "slept">
                            {:path [:english :past :1sing]
                             :merge-fn
                             (fn [val]
                               {:synsem {:cat :verb
                                         :subcat {:1 {:agr {:number :sing
                                                            :person :1st}}}}
                                :english {:infl :past
                                          :english (get-in val [:english :past :1sing])}})}

                            {:path [:english :past :2sing]
                             :merge-fn
                             (fn [val]
                               {:synsem {:cat :verb
                                         :subcat {:1 {:agr {:number :sing
                                                            :person :2nd}}}}
                                :english {:infl :past
                                          :english (get-in val [:english :past :2sing])}})}

                            {:path [:english :past :3sing]
                             :merge-fn
                             (fn [val]
                               {:synsem {:cat :verb
                                         :subcat {:1 {:agr {:number :sing
                                                            :person :3rd}}}}
                                :english {:infl :past
                                          :english (get-in val [:english :past :3sing])}})}

                            {:path [:english :past :1plur]
                             :merge-fn
                             (fn [val]
                               {:synsem {:cat :verb
                                         :subcat {:1 {:agr {:number :plur
                                                            :person :1st}}}}
                                :english {:infl :past
                                          :english (get-in val [:english :past :1plur])}})}

                            {:path [:english :past :2plur]
                             :merge-fn
                             (fn [val]
                               {:synsem {:cat :verb
                                         :subcat {:2 {:agr {:number :plur
                                                            :person :2nd}}}}
                                :english {:infl :present
                                          :english (get-in val [:english :past :2plur])}})}

                            {:path [:english :past :3plur]
                             :merge-fn
                             (fn [val]
                               {:synsem {:cat :verb
                                         :subcat {:3 {:agr {:number :plur
                                                            :person :3rd}}}}
                                :english {:infl :past
                                          :english (get-in val [:english :past :3plur])}})}


                            {:path [:english :past]
                             :merge-fn
                             (fn [val]
                               {:synsem {:cat :verb}
                                :english {:infl :past
                                          :english (get-in val [:english :past])}})}
                            ;; </2. past exceptions: e.g. "sleep" -> "slept">

                            ;; <3. present exceptions: e.g. "be" -> "am">
                            {:path [:english :present :1sing]
                             :merge-fn
                             (fn [val]
                               {:synsem {:cat :verb
                                         :subcat {:1 {:agr {:number :sing
                                                            :person :1st}}}}
                                :english {:infl :present
                                          :english (get-in val [:english :present :1sing])}})}

                            {:path [:english :present :2sing]
                             :merge-fn
                             (fn [val]
                               {:synsem {:cat :verb
                                         :subcat {:1 {:agr {:number :sing
                                                            :person :2nd}}}}
                                :english {:infl :present
                                          :english (get-in val [:english :present :2sing])}})}

                            {:path [:english :present :3sing]
                             :merge-fn
                             (fn [val]
                               {:synsem {:cat :verb
                                         :subcat {:1 {:agr {:number :sing
                                                            :person :3rd}}}}
                                :english {:infl :present
                                          :english (get-in val [:english :present :3sing])}})}

                            {:path [:english :present :1plur]
                             :merge-fn
                             (fn [val]
                               {:synsem {:cat :verb
                                         :subcat {:1 {:agr {:number :plur
                                                            :person :1st}}}}
                                :english {:infl :present
                                          :english (get-in val [:english :present :1plur])}})}

                            {:path [:english :present :2plur]
                             :merge-fn
                             (fn [val]
                               {:synsem {:cat :verb
                                         :subcat {:1 {:agr {:number :plur
                                                            :person :2nd}}}}
                                :english {:infl :present
                                          :english (get-in val [:english :present :2plur])}})}

                            {:path [:english :present :3plur]
                             :merge-fn
                             (fn [val]
                               {:synsem {:cat :verb
                                         :subcat {:1 {:agr {:number :plur
                                                            :person :3rd}}}}
                                :english {:infl :present
                                          :english (get-in val [:english :present :3plur])}})}


                            {:path [:english :present]
                             :merge-fn
                             (fn [val]
                               {:synsem {:cat :verb}
                                :english {:infl :present
                                          :english (get-in val [:english :present])}})}
                            ;; </3. present exceptions: e.g. "be" -> "am"

                            {:path [:english :participle]
                             :merge-fn
                             (fn [val]
                               {:synsem {:cat :verb}
                                :english {:infl :participle
                                          :english (get-in val [:english :participle])}})}


                            ])]
        (concat result (exception-generator (rest lexicon)))))))

(defn phonize [a-map a-string]
  (let [common {:phrasal false}]
    ;; TODO: remove support for either list-of-maps - too confusing. Instead, just require a list of maps.
    ;; TODO: compare with counterpart function: (italiano/phonize): there is an additional cond stanza in the latter
    ;; that is not present here.
    (cond (or (vector? a-map) (seq? a-map))
          (map (fn [each-entry]
                 (phonize each-entry a-string))
               a-map)

          true
          (unifyc a-map
                  {:english {:english a-string}}
                  common))))


