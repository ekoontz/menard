# Number expressions, take 1

Number expressions are phrases like "vier en twentig" or "twenty four".

## Think of a sentence

For example, "vier en twintig".

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

## Part 3: do(until(remove-bad-parses))

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

## Part 4: do(until(generate-only-good-sentences))

Starting with the semantics `semantics-of-parse` of the parse from the last section, 
create: 
```
{:sem semantics-of-parse}
``` 

and iterate generating on this, adding information to this starting spec until:
- It generates only the desired surface forms as expected.
- Is reasonably "clean", i.e. anything unneeded for the correct generation is
  is removed.

Done with this:
```
git:65956c87
(u/pprint spec)
{:phrasal true,
 :comp {:phrasal :top},
 :cat :noun,
 :subcat [],
 :sem
 {:pred :top,
  :ref {:number :plur},
  :mod [],
  :arg :top,
  :quant :add,
  :context :unspec}}
babylon.nederlands> (count (take 2 (repeatedly #(-> spec generate morph println))))
negen en twentig honden
zes en vijftig meisjes
2
babylon.nederlands> (count (take 2 (repeatedly #(-> spec generate (u/get-in [:sem]) u/pprint println))))
{:pred :loneliness, :arg2 {:pred 8, :times 1}, :ref {:number :plur}, :mod [], :arg :top, :quant :add, :context :unspec, :arg1 {:pred 5, :times 10}}
{:pred :solution, :arg2 {:pred 5, :times 1}, :ref {:number :plur}, :mod [], :arg :top, :quant :add, :context :unspec, :arg1 {:pred 5, :times 10}}
2
```

# Number expressions, take 2

This time, we're going to have the numbers be adjectives, not determiners, so that
we can handle things like 'de drie musketiers'.

## Think of a sentence

For example, "de vier en twintig vogels".

## Part 1: get "de vier en twintig vogels" parsing

### Change numbers expressions to be 'adjective-phrases'

...

Parts 1,2,3 done:

```
git:639d1bb3
babylon.nederlands> (->> "de vier en twentig honden" parse (map syntax-tree))
("[np .de +[nbar .[number-expression-outer .vier +[number-expression-inner +en .twentig]] +honden]]")
babylon.nederlands> (->> "de vier en twentig honden" parse (map #(u/get-in % [:sem])) (map u/pprint))
({:pred :dog,
  :arg2 :top,
  :ref [[1] {:number [[2] :plur]}],
  :mod
  {:first
   {:arg2 {:pred 4, :mod [], :times 1},
    :arg [1],
    :quant [[3] :the],
    :arg1 {:pred 2, :mod [], :times 10}},
   :rest []},
  :arg :top,
  :quant [3],
  :context :unspec,
  :arg1 :top})
```

## Part 4: do(until(generate-only-good-sentences))

Starting with the semantics `semantics-of-parse` of the parse from the last section, 
create: 
```
{:sem semantics-of-parse}
``` 

and iterate generating on this, adding information to this starting spec until:
- It generates only the desired surface forms as expected.
- Is reasonably "clean", i.e. anything unneeded for the correct generation is
  is removed.


### status

Not complete yet but showing output at this stage:

```
git:1d3dd95b
babylon.nederlands> (-> "de vier en twentig honden"
                        parse 
                        first
                        ((fn [input-expression]
                           (let [spec {:sem (u/get-in input-expression [:sem])
                                       :cat :noun}]
                             {:input-expression (-> input-expression syntax-tree)
                              :generated-expression (-> spec generate syntax-tree)
                              :spec spec})))
                        u/pprint)

{:input-expression
 "[np .de +[nbar .[number-expression-outer .vier +[number-expression-inner +en .twentig]] +honden]]",
 :generated-expression "[np .de +[nbar .vijftig +honden]]",
 :spec
 {:sem
  {:pred :dog,
   :arg2 :top,
   :ref [[1] {:number [[2] :plur]}],
   :mod
   {:rest [],
    :first
    {:arg2 {:pred 4, :mod [], :times 1},
     :arg [1],
     :quant [[3] :the],
     :arg1 {:pred 2, :mod [], :times 10}}},
   :arg :top,
   :quant [3],
   :context :unspec,
   :arg1 :top},
  :cat :noun}}
babylon.nederlands> 
```

### status

Able to consistently roundtrip on a number-expression:

```
git:1d3dd95b
babylon.nederlands> (-> "twentig honden"
                        roundtrip)
{:input-expression "[nbar .twentig +honden]",
 :input-spec
 {:sem
  {:pred :dog,
   :arg2 :top,
   :ref [[1] {:number [[2] :plur]}],
   :mod [],
   :quant [[3] :top],
   :context :unspec,
   :arg1 :top},
  :mod
  {:first {:pred 2, :mod [], :times 10, :arg [1], :quant [3]},
   :rest []},
  :phrasal true,
  :cat :noun},
 :readability-divider "--------------------------------",
 :generated-expression "[nbar .twentig +honden]",
 :output-structure
 {:sem
  {:pred :dog,
   :arg2 :top,
   :ref [[4] {:number [[5] :plur]}],
   :mod [],
   :quant [[6] :top],
   :context :unspec,
   :arg1 :top},
  :mod
  {:first {:pred 2, :mod [], :times 10, :arg [4], :quant [6]},
   :rest []}}}
babylon.nederlands> 
```

Able to consistently roundrip:

```
git:c104f43d
(count (take 5 (repeatedly #(-> "de vier en twentig honden" roundtrip :generated-expression println))))
                        
[np .de +[nbar .[number-expression-outer .vier +[number-expression-inner +en .twentig]] +honden]]
[np .de +[nbar .[number-expression-outer .vier +[number-expression-inner +en .twentig]] +honden]]
[np .de +[nbar .[number-expression-outer .vier +[number-expression-inner +en .twentig]] +honden]]
[np .de +[nbar .[number-expression-outer .vier +[number-expression-inner +en .twentig]] +honden]]
[np .de +[nbar .[number-expression-outer .vier +[number-expression-inner +en .twentig]] +honden]]
5
```

Able to consistently roundrip:
```
(count (take 10 (repeatedly #(-> "de vier en twentig honden slapen" roundtrip :generated println))))
de vier en twentig honden slapen
de vier en twentig honden slapen
de vier en twentig honden slapen
de vier en twentig honden slapen
de vier en twentig honden slapen
de vier en twentig honden slapen
de vier en twentig honden slapen
de vier en twentig honden slapen
de vier en twentig honden slapen
de vier en twentig honden slapen
```

