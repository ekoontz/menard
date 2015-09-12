(ns babel.parse
 (:refer-clojure :exclude [get-in resolve find]))

(require '[clojure.string :as str])
(require '[clojure.tools.logging :as log])

(require '[babel.over :as over])
(require '[dag-unify.core :refer (get-in strip-refs)])

;; for now, using a language-independent tokenizer.
(def tokenizer #"[ ']")

(declare toks2)

(defn toks [s lexicon lookup]
  (let [lexicon (if (future? lexicon) @lexicon lexicon)]
    (vec (toks2 (str/split s tokenizer) lexicon lookup))))

(defn toks2 [tokens lexicon lookup]
  "like (toks), but use lexicon to consolidate two initial tokens into one. may consolidate larger groups than two in the future."
  (cond (nil? tokens) nil
        (empty? tokens) nil
        (> (.size tokens) 1)
        ;; it's two or more tokens, so try to combine the first and the second of them:
        (let [looked-up (lookup (str (first tokens) " " (second tokens)))]
          (if (not (empty? looked-up))
            ;; found a match by combining first two tokens.
            (cons looked-up
                  (toks2 (rest (rest tokens)) lexicon lookup))
            ;; else, no match: consider the first token as a standalone token and continue.
            (cons (lookup (first tokens))
                  (toks2 (rest tokens) lexicon lookup))))
        ;; only one token left: look it up.
        (= (.size tokens) 1)
        (list (lookup (first tokens)))
        true
        nil))

(defn create-unigram-map [args index]
  (if (< index (.size args))
    (merge
     {[index (+ 1 index)]
      (subvec args index (+ 1 index))}
     (create-unigram-map args (+ 1 index)))))

(defn create-bigram-map [args index grammar]
  (if (< (+ 1 index) (.size args))
    (let [left-side (subvec args index (+ 1 index))
          right-side (subvec args (+ 1 index) (+ 2 index))]
      (merge
       {[index (+ 2 index)]
        (over/over grammar left-side right-side)}
       (create-bigram-map args (+ index 1) grammar)))
    (create-unigram-map args 0)))

(declare over)

(defn create-trigram-map [args index grammar bigrams]
  (if (< (+ 2 index) (.size args))
    (do
      (merge
       {[index (+ 3 index)]
        (lazy-cat
         ;; [ a b | c ]
         (let [left-parse (get bigrams [index (+ 2 index)])
               right-parse (get bigrams [(+ 2 index) (+ 3 index)])]
           (if (and (not (empty? left-parse))
                    (not (empty? right-parse)))
             (over grammar left-parse right-parse)))

         ;; [ a | b c ]
         (let [left-parse (get bigrams [index (+ 1 index)])
               right-parse (get bigrams [(+ 1 index) (+ 3 index)])]
           (if (and (not (empty? left-parse))
                    (not (empty? right-parse)))
             (over grammar left-parse right-parse))))}
       (create-trigram-map args (+ index 1) grammar bigrams)))
    bigrams))

(defn over [grammar left right]
  "opportunity for additional logging before calling the real (over)"
  (over/over grammar left right))

(defn create-ngram-map [args left ngrams grammar split-at x]
  (log/debug (str "create-ngram-map: left:" left ";split-at:" split-at "; size:" (.size args) "; x:" x))
  (if (< (+ left (- split-at 2))
         (/ (.size args) 2))
    (lazy-cat
     (let [left-parses (get ngrams [left (+ left (- split-at 0))] '())
           right-parses (get ngrams [(+ left split-at 0) (- (.size args) 0)] '())]
       (if (and (not (empty? left-parses))
                (not (empty? right-parses)))
         (over grammar left-parses right-parses)))
     (create-ngram-map args left ngrams grammar (+ 1 split-at) x))))

(defn create-xgram-map [args x index grammar & [nminus1grams runlevel]]
  (cond (= x 0) {}
        (= x 1) (create-unigram-map args index)
        (= x 2) (create-bigram-map args index grammar)

        true (let [nminus1grams (if nminus1grams nminus1grams
                                    (create-xgram-map args (- x 1) 0 grammar))]
               (cond
                (= x 3) (create-trigram-map args index grammar nminus1grams)
                (< (+ x index) (+ 1 (.size args)))
                (let [runlevel (if runlevel runlevel 0)]
                  (log/debug (str "create-xgram-map: x=" x "; index=" index "; runlevel=" runlevel))
                  (log/debug (str "  -> create-ngram-map(index:" index ";split-at: " 1 ";x:" x))
                  (log/debug (str "  -> create-xgram-map(x:" x "; index:" (+ 1 index)))
                  (create-xgram-map args x (+ index 1) grammar 

                                    ;; combine the parses for this span from [index to (+ x index))...
                                    (merge 

                                     {[index (+ x index)]
                                      (create-ngram-map args index nminus1grams grammar 1 x)}

                                     ;; .. with parses of all of the proceeding consituent parts.
                                     nminus1grams)
                                    (+ 1 runlevel)))
                true
                nminus1grams))))

;; TODO: move tokenization to within lexicon.
(defn parse [arg lexicon lookup grammar]
  "return a list of all possible parse trees for a string or a list of lists of maps (a result of looking up in a dictionary a list of tokens from the input string)"
  (cond (string? arg)
        (parse (toks arg lexicon lookup) lexicon lookup grammar)

        (and (vector? arg)
             (empty? (rest arg)))
        (first arg)

        (vector? arg)
        ;; return the parse of the whole expression.
        ;; TODO: if a parse for the whole expression is not found,
        ;; return the largest subparse(s).
        (get (create-xgram-map arg (.size arg) 0 grammar)
             [0 (.size arg)])
        true
        :error))

;(log/info (str "parse: " (it/fo (parse "il gatto ha dormito" it/lexicon it-g/grammar))))
