(ns babel.parse
 (:refer-clojure :exclude [get-in resolve find])
 (:require
  [babel.over :as over]
  [clojure.string :as string]
  #?(:clj [clojure.tools.logging :as log])
  #?(:cljs [babel.logjs :as log])
  [dag_unify.core :refer (get-in strip-refs)]))

;; for now, using a language-independent tokenizer.
(def tokenizer #"[ ']")
(declare over)
(declare toks2)

(defn toks [s lookup]
  (let [tokens (string/split s tokenizer)
        tokens2 (toks2 tokens (count tokens))]
    (pmap (fn [token-vector]
            (pmap lookup token-vector))
         tokens2)))

(defn toks2 [tokens n]
  "group tokens together into every possible sequence of n or less tokens."
  (cond
    (< n 1) nil
    (= n 1) (vec (list tokens))
    (> n (count tokens)) (toks2 tokens (count tokens))

    (= (count tokens) n) ;; ([a b]; n = 2) => [["a b"] ["a" "b"]]
    (cons [(string/join " " tokens)]
          (toks2 tokens (- n 1)))
    
    (> (count tokens) n) ;; ([a b c]; n = 2) => [["a b" "c"] ["a" "b c"]]
    (concat (let [first-token (string/join " " (subvec tokens 0 n))]
              (pmap #(vec (cons first-token %))
                    (toks2 (subvec tokens n (count tokens))
                           n)))
            (let [last-token (string/join " " (subvec tokens (- (count tokens) n) (count tokens)))]
              (pmap #(vec (concat % (list last-token)))
                    (toks2 (subvec tokens 0 (- (count tokens) n))
                           n)))
            (toks2 tokens (- n 1)))
    true
    tokens))

(defn create-unigram-map [args index]
  (if (< index (count args))
    (merge
     {[index (+ 1 index)]
      (subvec args index (+ 1 index))}
     (create-unigram-map args (+ 1 index)))))

(defn create-bigram-map [args index grammar]
  (log/debug (str "create-bigram-map: args count: " (count args)))
  (if (< (+ 1 index) (count args))
    (let [left-side (subvec args index (+ 1 index))
          right-side (subvec args (+ 1 index) (+ 2 index))]
      (log/debug (str "create-bigram-map: size of left-side: " (count left-side)))
      (log/debug (str "create-bigram-map: size of right-side: " (count right-side)))
      (merge
       {[index (+ 2 index)]
        (over grammar
              (list (first left-side))
              (list (first right-side)))}
       (create-bigram-map args (+ index 1) grammar)))
    (create-unigram-map args 0)))

(declare over)

(defn create-trigram-map [args index grammar bigrams]
  (if (< (+ 2 index) (count args))
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
  (log/debug (str "parse/over: grammar: " (count grammar) " left: " (type left) "; right: " (type right)))
  (over/over grammar left right))

(defn create-ngram-map [args left ngrams grammar split-at x]
  (log/debug (str "create-ngram-map: left:" left ";split-at:" split-at "; size:" (count args) "; x:" x))
  (if (< (+ left (- split-at 2))
         (/ (count args) 2))
    (lazy-cat
     (let [left-parses (get ngrams [left (+ left (- split-at 0))] '())
           right-parses (get ngrams [(+ left split-at 0) (- (count args) 0)] '())]
       (if (and (not (empty? left-parses))
                (not (empty? right-parses)))
         (over grammar left-parses right-parses)))
     (create-ngram-map args left ngrams grammar (+ 1 split-at) x))))

(defn create-xgram-map [args x index grammar & [nminus1grams runlevel]]
  (cond (= x 0) {}
        (= x 1) (create-unigram-map args index)
        (= x 2) (let [bigram-map (create-bigram-map args index grammar)]
                  (log/trace (str "create-xgram-map: bigram-map: " bigram-map))
                  (log/debug (str "create-xgram-map: keys: " (keys bigram-map)))
                  (log/debug (str "create-xgram-map: vals: "
                                  (string/join ";"
                                               (map
                                                (fn [val]
                                                  (string/join ","
                                                               (map (fn [x]
                                                                      (cond
                                                                        (map? x)
                                                                        (:rule x)
                                                                        true (str
                                                                              "not map:"
                                                                              (type x))))
                                                                    val)))
                                                (vals bigram-map)))))
                  bigram-map)

        true (let [nminus1grams (if nminus1grams nminus1grams
                                    (create-xgram-map args (- x 1) 0 grammar))]
               (cond
                (= x 3) (create-trigram-map args index grammar nminus1grams)
                (< (+ x index) (+ 1 (count args)))
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
(defn parse [arg lookup grammar]
  "return a list of all possible parse trees for a string or a list of lists of maps (a result of looking up in a dictionary a list of tokens from the input string)"
  (cond (string? arg)
        (let [tokens (toks arg lookup)]
          (parse tokens lookup grammar))

        (and (vector? arg)
             (empty? (rest arg)))
        (first arg)

        (vector? arg)
        ;; return the parse of the whole expression.
        ;; TODO: if a parse for the whole expression is not found,
        ;; return the largest subparse(s).
        (get (create-xgram-map arg (count arg) 0 grammar)
             [0 (count arg)])

        (seq? arg)
        (mapcat #(parse (vec %) lookup grammar)
                arg)
        
        ;; TODO: throw exception here.
        true
        (str "unexpected input: type: " (type arg))))
