# Intro

Semantic modification must satisfy a few constraints:

- must be no cycles in graph (a.)
- must be language-independent (b.)
- must allow modifiers to contrain the objects they refer to (c.)
- must be direction-independent (works for either generation or parsing) (d.)
- must compose solely through consing, so that unification works. (e.)

Below we describe a proposed solution that meets these requirements.

# Summary

- The `:ref` feature is used to avoid cycles (a. above). This `:ref` is used by `[:mod :arg]` to refer to what is being modified.

- Two new ug meta-rules:
  - `nest-mod`: 
     - the :sem of the `:head` and the `:sem` of the parent are *distinct*: `[:head :sem]` != `[:sem]`.
     - the :mod of the head is moved inside the parent's `:sem`: `[:head :mod]` = `[:sem :mod]`.
  - `cons-mod`:
     - the :sem of the :head and the :sem of the parent are *distinct*: `[:head :sem]` = `[:sem]`.
	 - the :mod of the parent is equal to the cons of:
	   - :sem of the comp: `[:head :mod :first]` = `[:comp :sem]`
	   - :mod of the head. `[:head :mod :rest]`  = `[:comp :mod]`.
  
- English rules' use of these meta rules:
  - `nest-mod`: `np`,`vp`,..
  - `cons-mod`: `n-mod` (a.k.a. `nbar`),..
  - `no-mod`:   `s`,`s/`, `cp`,..


# Lexical entries

## Nouns

```
{:cat :noun
 :sem {:pred :dog
       :ref {:spatial true}
       :mod []}}
```

## Adjectives

```
{:cat :adj
 :sem {:pred :orange
       :arg {:spatial true}}}
```

## Verbs

```
{:cat :verb
 :sem {:pred :sleeps
       :subj {:animate true (c.)
	          :ref [1]}}
 :subcat {:1 {:cat :noun
              :sem {:ref [1]}}
	      :2 []}}
```

# Rules

`n-mod` is a rule where :sem and :mod are siblings (`cons-mod`): 

```
{:rule "nbar"
 :comp [1]
 :sem [2] {:ref [3]}
 :mod <[4]{:arg [3]} [5]>
 :head {:sem [2]
        :mod [5]
        :subcat {:1 [1]
                 :2 []}}}
 :comp {:sem [4]}}
```

`np`, by contrast, is a rule where :mod is *within* :sem (`nest-mod`):

```
{:rule "np"
 :sem {:mod [1]
       :pred [2]
	   :ref [3]}
 :head {:sem {:pred [2]
              :ref [3]}
        :mod [1]}}
```

# Example Expression Semantics

Here we'll use the example sentence "the small orange dog that you see sleeps quietly" and its constituent phrases:

## `[s [np the [nbar [nbar small [nbar orange dog]] [cp that [s/ you see]]]] [vp sleeps quietly]]`

The `:mod` of the entire expression is nested within the `:sem`:

```
{:sem {:pred :sleep
       :ref [2]
       :subj {:pred :dog
              :ref [1]}
              :mod <{:pred :see
                     :obj [1]
                     :subj {:pred :you}}>}
                    {:pred :small
                     :arg [1]}
                    {:pred :orange
                     :arg [1]}>}
       :mod <{:pred :quiet
	          :arg [2]}>}}
```

## `[np the [nbar [nbar small [nbar orange dog]] [cp that [s/ you see]]]]`

```
{:sem {:pred :dog
       :ref [1]}
       :mod <{:pred :see
              :obj [1]
              :subj {:pred :you}}>}
             {:pred :small
              :arg [1]}
             {:pred :orange
              :arg [1]}>}
```

## `[nbar [nbar small [nbar orange dog]] [cp that [s/ you see]]]`

The outer `n-mod`: `:sem` and `:mod` are siblings:

```
{:sem {:pred :dog
       :ref [1]}
 :mod <{:pred :see
        :obj [1]
        :subj {:pred :you}}>}
       {:pred :small
        :arg [1]}
       {:pred :orange
       :arg [1]}>}
```

## `[nbar [nbar small [nbar orange dog]]]`

The middle `n-mod`: `:sem` and `:mod` are also siblings:

```
{:sem {:pred :dog
       :ref [1]}
 :mod <{:pred :small
       :arg [1]}
       {:pred :orange
       :arg [1]}>}
```

## `[nbar orange dog]`

The inner `nmod`: `:sem` and `:mod` are once more siblings:

```
{:sem {:pred :dog
       :ref [1]}
 :mod <{:pred :orange
        :arg [1]}>}
```

## `[cp that [s/ you see]]`

```
{:subcat {:1 {:sem [1]}}
 :sem {:pred :see
       :obj [1]
       :subj {:pred :you}}>}
```

## `[vp sleeps quietly]`

```
{:subcat {:1 {:sem [1]
          :2 []}}
 :sem {:pred :sleeps
       :subj [1]}}
```
