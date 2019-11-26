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

- Three new ug meta-rules:
  - `nest-mod`: 
     - the :sem of the `:head` and the `:sem` of the parent are *distinct*: `[:head :sem]` != `[:sem]`.
     - the :mod of the head is moved inside the parent's `:sem`: `[:head :mod]` = `[:sem :mod]`.
  - `cons-mod`:
     - the :sem of the :head and the :sem of the parent are *identical*: `[:head :sem]` = `[:sem]`.
	 - the :mod of the parent is equal to the cons of:
	   - :sem of the comp: `[:head :mod :first]` = `[:comp :sem]`
	   - :mod of the head. `[:head :mod :rest]`  = `[:comp :mod]`.
  - `cons-and-nest-mod`:
    - the `:sem` of the `:head` and the `:sem` of the parent are *identical*: `[:head :sem]` = `[:sem]`.
	- this `[:sem :mod]` is a list with one member: `[:comp :mod]`.

- English rules' use of these meta rules:
  - `nest-mod`: `np`,`vp`.
  - `cons-mod`: `nmod` (a.k.a. `nbar`),..
  - `no-mod`:   `s`,`s/`, `cp`,..
  - `cons-and-nest-mod`: `vmod`.

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

## Adverbs

```
{:cat :adv
 :sem {:pred :quiet
       :arg {:activity true}}}
```

## Verbs

```
{:cat :verb
 :sem {:pred :sleep
       :ref {:activity true}
       :subj {:animate true (c.)
	          :ref [1]}}
 :subcat {:1 {:cat :noun
              :sem {:ref [1]}}
	      :2 []}}
```

# Rules

`nmod` is a rule where :sem and :mod are siblings (`cons-mod`): 

```
{:rule "nmod"
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

# Example Expression Representations

Here we'll use the example sentence "the small orange dog that you see hunts a mouse stealthily" and its constituent phrases:

## `[s [np the [nmod [nmod small [nmod orange cat]] [cp that [s/ you see]]]] [vmod [vp hunts a mouse] stealthily]]`

`s` is neither `nest-mod`, `cons-mod`, nor `:cons-and-nest-mod`: neither child should have a
`:mod` feature. This is enforced with `:mod ::unspec`:

```
{:sem {:pred :hunts
	   :obj {:ref [3]
             :pred :mouse
	         :mod <{:ref [3]
			        :pred :grey}>}
       :ref [2]
       :subj {:pred :cat
              :ref [1]}
              :mod <{:pred :see
                     :obj [1]
                     :subj {:pred :you}}>}
                    {:pred :small
                     :arg [1]}
                    {:pred :orange
                     :arg [1]}>}
       :mod <{:pred :stealth
	          :arg [2]}>}
 :mod ::unspec}
 :head {:mod ::unspec}
 :comp {:mod ::unspec}}
```

## `[np the [nmod [nmod small [nmod orange cat]] [cp that [s/ you see]]]]`

`np` is `nest-mod`:

```
{:sem {:pred :cat
       :ref [1]}
       :mod <{:pred :see
              :obj [1]
              :subj {:pred :you}}>}
             {:pred :small
              :arg [1]}
             {:pred :orange
              :arg [1]}>
 :mod ::unspec}
 :head {:mod ::unspec}
 :comp {:mod ::unspec}}
```

## `[nmod [nmod small [nmod orange cat]] [cp that [s/ you see]]]`

`nmod` is `cons-mod`; as such, the outer `nmod`: `:sem` and `:mod` are siblings:

```
{:sem {:pred :cat
       :ref [1]}
 :mod <{:pred :see
        :obj [1]
        :subj {:pred :you}}>}
       {:pred :small
        :arg [1]}
       {:pred :orange
       :arg [1]}>}
```

## `[nmod [nmod small [nmod orange cat]]]`

The middle `nmod`: `:sem` and `:mod` are also siblings:

```
{:sem {:pred :cat
       :ref [1]}
 :mod <{:pred :small
        :arg [1]}
       {:pred :orange
        :arg [1]}>}
```

## `[nmod orange cat]`

The inner `nmod`: `:sem` and `:mod` are once more siblings:

```
{:sem {:pred :cat
       :ref [1]}
 :mod <{:pred :orange
        :arg [1]}>}
```

## `[cp that [s/ you see]]`

Like `s`. `cp` is neither `nest-mod`, `cons-mod`, nor `cons-and-nest-mod`: neither child should have a 
`:mod` feature. This is enforced with `:mod ::unspec` on the rule.

```
{:subcat {:1 {:sem [1]}}
 :sem {:pred :see
       :obj [1]
       :subj {:pred :you}}>}`
 :mod ::unspec
 :head {:mod ::unspec}
 :comp {:mod ::unspec}}
```

## `[vmod [vp hunts a grey mouse] stealthily]`

`vmod` is `cons-and-nest-mod`:

```
{:subcat {:1 {:cat :noun}
          :2 []}}
 :sem {:pred :hunts
       :subj [1]
	   :obj {:ref [3]
             :pred :mouse
	         :mod <{:ref [3]
			        :pred :grey}>}
	   :ref [2]
	   :mod <{:pred :stealth
	          :arg [2]}>}}
 :mod ::unspec
 :head {:mod ::unspec}
 :comp {:mod ::unspec}}
```

## `[vp hunts a grey mouse]`

Like `s` and `cp`, `vp` is neither `nest-mod`, `cons-mod` nor `cons-and-nest-mod`: neither child should have a
`:mod` feature. This is enforced with `:mod ::unspec`.

```
{:subcat {:1 {:cat :noun}
          :2 []}}
 :sem {:pred :hunts
       :subj [1]
	   :obj {:ref [3]
             :pred :mouse
	         :mod <{:ref [3]
			        :pred :grey}>}
	   :ref [2]
 :mod ::unspec
 :head {:mod ::unspec}
 :comp {:mod ::unspec}}
```
