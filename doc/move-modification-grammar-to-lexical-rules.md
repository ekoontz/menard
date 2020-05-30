Consider the existing rule:

```
 {:rule :intensifier-semantics
  :if {:cat :intensifier}
  :then [(let [adj-mod (atom :top)
               adj-pred (atom :top)
               intensifier-sem (atom :top)]
           {:parent-sem {:pred adj-pred
                         :mod {:first intensifier-sem
                               :rest adj-mod}}
            :sem intensifier-sem
            :subcat {:1 {:cat :adjective
                         :sem {:pred adj-pred
                               :number? false
                               :mod adj-mod}}
                     :2 []}})]}
```

This is a step in the right direction: it should be further generalized.
It should be used for all cases where we need to build a list of modifiers
and then, after all the modifiers are added, terminate it by creating
a new :sem, whose contents are:

1. the head's :mod.
2. all of the keys in the head's :sem.


Consider this expression P:

```
 P {:sem {:mod <1,2,3>
 |\       :pred 5
 | \      :obj 6, ...}
 |  \
 c   P' {:mod <1,2,3>
     | \ :sem 4:{:pred 5,
     |  \        :obj 6, ...}
     |   \
     |    \
 c{:sem 1} h {:sem 4
           |\ :mod <2,3>}
           | \ 
   c{:sem 2}  \
               \
                h {:sem 4
                |\ :mod <3>}
                | \
                |  \
        c{:sem 3}   h {:sem 4
                       :mod <>}
```

An example of P would be "de vier kleine vogels".

## About the topmost c

The top c of P does not modify the head (i.e. it
does not cons a value to the parent's :mod or the parent's :head :mod).

This c is derived via a 'terminator' lexical rule:

```
{:head-sem {:pred [1]
            :subj [2]
            :obj [3]
			:.. whatever else we need to copy from the head..}
 :parent-sem {:sem {:pred [1]
                    :obj [2]
			        :.. these same things we need to copy from the head.}}}
```

And then, in the grammar, P's rule inherits from this rule:

```
{:sem [1] {:mod [2]}
 :head {:mod [2]}
 :comp {:parent-sem [1]}
```

## About the c's below that (c{:sem 1})

All of these c's cons their semantics to the existing :mod from the head.
This c is derived via a 'cons' lexical rule:

```
{:sem [1]
 :head-mod [2]
 :parent-mod {:first [1]
              :rest [2]}
```

so that c's :sem ([2]) is cons'ed with the :mod of the head.


and in the grammar, the P' inherits from this rule:

```
{:mod [1] {:rest [2]}
 :head {:mod [2]}
 :comp {:parent-mod [1]}}
```

## Another example

Consider P:

```
 P {:sem 4:{:mod <1,2,3>
 |  \       :pred 5,
 |   \      :obj 6, ...}}
 |    \
 |     \
 |      \
 |       \
c{:sem 1} h {:sem 4
          |\ :mod <2,3>}
          | \ 
  c{:sem 2}  \
              \
               h {:sem 4
               |\ :mod <3>}
               | \
               |  \
       c{:sem 3}   h {:sem 4
                      :mod <>}
```

An example of P would be "drie kleine vogels".

This c is derived via a 'cons-and-terminator' rule:


```
```

and in the grammar, the P inherits from this rule:

```
```








