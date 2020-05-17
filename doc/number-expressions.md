# Number expressions

Number expressions are phrases like "vier en twentig" or "twenty four".

# Think of a sentence

For example, "vier en twintig".

# do(until(can-tokenize-all-words-with-correct-semantics))

Edit lexemes and lexical rules until you can analyze all the words in the expression
and that a subset of these lexemes are of the expected semantics.
It's ok if there are other incorrect analyses if said analyses are useful
in other expressions. These analyses will be filtered out during parsing in the
next step.

Done:
```
(->> (-> "vier en twentig" (clojure.string/split #" ")) (map analyze) flatten (map (fn [x] {:sem (u/get-in x [:sem]) :cat (u/get-in x [:cat])})))
({:sem {:pred :four, :base 1}, :cat :det}
 {:sem {:pred :and}, :cat :conjunction}
 {:sem {:pred :two, :base 10}, :cat :det})
babylon.nederlands> 
```

# do(until(a-parse-with-correct-semantics))

Edit rules and lexemes until you can parse the expression, and the semantics
of this expression are as expected.

## Part 1: get "en twentig" parsing

```
git: 511f3152
(->> 
  (-> "en twentig" parse) 
  (map #(u/get-in % [:sem])) 
  (map u/pprint))

=>

({:pred :two, :base 10})
```

## Part 2: get "[vier [en twentig]]" parsing

```
git:b4c284d7
babylon.nederlands> (->> (-> "vier en twentig" parse) (map syntax-tree))
("[number-expression-outer .vier +[number-expression-inner +en .twentig]]")
babylon.nederlands> 
```

# do(until(remove-bad-parses))

Edit rules and lexemes if necessary to remove bad parses of the expression (expressions with 
the undesired semantics).

```
git:
babylon.nederlands> (->> (-> "vier en twentig" parse) (map syntax-tree))
("[number-expression-outer .vier +[number-expression-inner +en .twentig]]")
babylon.nederlands> (->> (-> "vier en twentig" parse) (map #(u/get-in % [:sem])) (map u/pprint))
({:pred :add,
  :arg2 {:pred :four, :base 1},
  :arg1 {:pred :two, :base 10}})
babylon.nederlands> 
```

# do(until(generate-only-good-sentences))

Starting with the semantics `semantics-of-parse` of the parse from the last section, 
create: 
```
{:sem semantics-of-parse}
``` 

and iterate generating on this, adding information to this starting spec until:
- It generates only the desired surface forms as expected.
- Is reasonably "clean", i.e. anything unneeded for the correct generation is
  is removed.


