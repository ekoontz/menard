# Number expressions, take 1

Number expressions are phrases like "vier en twintig".

## Think of a sentence

For example, "vier en twintig".

## Part 1: get "en twintig" parsing

```
git: 511f3152
(->> 
  (-> "en twintig" parse) 
  (map #(u/get-in % [:sem])) 
  (map u/pprint))

=>

({:pred :two, :base 10})
```

## Part 2: get "[vier [en twintig]]" parsing

```
git:b4c284d7
menard.nederlands> (->> (-> "vier en twintig" parse) (map syntax-tree))
("[number-expression-outer .vier +[number-expression-inner +en .twintig]]")
menard.nederlands> 
```

## Part 3: do(until(remove-bad-parses))

Edit rules and lexemes if necessary to remove bad parses of the expression (expressions with 
the undesired semantics).

```
git:
menard.nederlands> (->> (-> "vier en twintig" parse) (map syntax-tree))
("[number-expression-outer .vier +[number-expression-inner +en .twintig]]")
menard.nederlands> (->> (-> "vier en twintig" parse) (map #(u/get-in % [:sem])) (map u/pprint))
({:pred :add,
  :arg2 {:pred :four, :base 1},
  :arg1 {:pred :two, :base 10}})
menard.nederlands> 
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
{:phrasal? true,
 :comp {:phrasal? :top},
 :cat :noun,
 :subcat [],
 :sem
 {:pred :top,
  :ref {:number :plur},
  :mod [],
  :arg :top,
  :quant :add,
  :context :unspec}}
menard.nederlands> (count (take 2 (repeatedly #(-> spec generate morph println))))
negen en twintig honden
zes en vijftig meisjes
2
menard.nederlands> (count (take 2 (repeatedly #(-> spec generate (u/get-in [:sem]) u/pprint println))))
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
menard.nederlands> (->> "de vier en twintig honden" parse (map syntax-tree))
("[np .de +[nbar .[number-expression-outer .vier +[number-expression-inner +en .twintig]] +honden]]")
menard.nederlands> (->> "de vier en twintig honden" parse (map #(u/get-in % [:sem])) (map u/pprint))
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
menard.nederlands> (-> "de vier en twintig honden"
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
 "[np .de +[nbar .[number-expression-outer .vier +[number-expression-inner +en .twintig]] +honden]]",
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
menard.nederlands> 
```

### status

Able to consistently roundtrip on a number-expression:

```
git:1d3dd95b
menard.nederlands> (-> "twintig honden"
                        roundtrip)
{:input-expression "[nbar .twintig +honden]",
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
  :phrasal? true,
  :cat :noun},
 :readability-divider "--------------------------------",
 :generated-expression "[nbar .twintig +honden]",
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
menard.nederlands> 
```

Able to consistently roundrip:

```
git:c104f43d
(count (take 5 (repeatedly #(-> "de vier en twintig honden" roundtrip :generated-expression println))))
                        
[np .de +[nbar .[number-expression-outer .vier +[number-expression-inner +en .twintig]] +honden]]
[np .de +[nbar .[number-expression-outer .vier +[number-expression-inner +en .twintig]] +honden]]
[np .de +[nbar .[number-expression-outer .vier +[number-expression-inner +en .twintig]] +honden]]
[np .de +[nbar .[number-expression-outer .vier +[number-expression-inner +en .twintig]] +honden]]
[np .de +[nbar .[number-expression-outer .vier +[number-expression-inner +en .twintig]] +honden]]
5
```

# Number expressions, take 3

Take 2 looks good correctness-wise, except it's rather ugly: the ug
rule: `addition-semantics` would be better handled lexically. In other
words, in the expression "vier en twintig" the Dutch conjunction 'en'
should subcategorize for its arguments "vier" and "twintig".

## Think of a sentence

For example, "vier en twintig kleine honden slapen"


## Part 1: get "vier en twintig kleine honden slapen"

```
git:4cceaf60
(->> "vier en twintig kleine honden slapen" parse (map syntax-tree))
("[s(:present-simple) .[np1 .[number-expression-outer .vier +[number-expression-inner +en .twintig]] +[np1 .kleine +honden]] +slapen]")
```

Changes since take 2:

- add new indefinite plural article 'wat'('some').
- 

## Part 2: do(until(generate-only-good-sentences))

Using this spec:

```
{:sem
 {:tense :present,
  :aspect :simple,
  :pred :sleep,
  :mod [],
  :subj
  {:pred :dog,
   :ref {:number :plur},
   :mod
   {:rest {:first {:pred :small, :mod []}, :rest []},
    :first
    {:arg2 {:pred 4, :mod [], :times 1},
     :pred :times,
     :arg1 {:pred 2, :mod [], :times 10}}},
   :quant :some,
   :context :unspec},
  :obj :unspec},
 :mod :top,
 :phrasal? true,
 :subcat [],
 :cat :verb}
```

We get the following expressions:

```
vier en twintig kleine honden slapen
vier en twintig wat kleine honden slapen
wat vier en twintig kleine honden slapen
```

The second (the ones with 'wat' in the middle) is bad. Its parse is:

```
[s(:present-simple) .[np1 .[number-expression-outer .vier +[number-expression-inner +en .twintig]] +[np2 .wat +[nbar .kleine +honden]]] +slapen]
```

Also the third sounds wrong too. Probably instead of:
  "wat vier en twintig kleine honden slapen", 
it should be:
  "ongeveer vier en twintig kleine honden slapen".

A slightly more general spec:

```
{:sem
 {:tense :present,
  :aspect :simple,
  :pred :sleep,
  :mod [],
  :subj
  {:ref {:number :plur},
   :mod
   {:rest {:first {:mod []}, :rest []},
    :first
    {:arg2 {:mod [], :times 1},
     :pred :times,
     :arg1 {:mod [], :times 10}}},
   :context :unspec},
  :obj :unspec},
 :phrasal? true,
 :subcat [],
 :cat :verb}
```

reveals some additional overgeneralizations:

```
verdrietig en klein opgewondene katten slapen
jouw klein en veertig acht gebeiden slapen
sterk en vijftig uw slimme gelden slapen
jouw twalf en verlegen eenzaamme dagen slapen
zijn klein en ernstig ernstige dagen slapen
hun verwarrend en ongerust nul vogels slapen
bedroefd en stom uw bedroefde levens slapen
verlegen en klein jouw verwarde haren slapen
twalf en verdrietig nieuwsgierige levens slapen
elf en waar teleurgestelde boeken slapen
```

After putting more constraints on number-expressions:

```
git:1faa6595
(count (take 5 (repeatedly #(-> expressions (nth 21) generate morph println))))
twalf en twalf hun verlegene dagen slapen
elf en twalf wat eenzaamme overheden slapen
vier en vijftig zeven haren slapen
die zeven en twalf zes stoelen slapen
twee en twintig verdrietige honden slapen
```
