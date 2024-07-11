# English modals

## Terminology

I consider modals as being one of the following:

- verbs like "try", "want", "hope" that take an infinitive verb phrase, e.g. "try to study linguistics"
- verbs like "will", "would" that take a base verb phrase e.g. "would study linguistics"

For completeness, the "to" in infinitive verb phrases are modal type :to, and non-modal verbs are of type :none (i.e. not modals).

## Examples

Consider these examples:

|-|-|
| she studies      |          [she      {:infl :present :modal :none}studies] |
| she will  study  |          [she      [vp {:infl :base   :modal :base}will           [vp {:infl :base :modal :none}study]]] |
| she wants to study |        [she      [vp {:infl :present :modal :infinitive}wants   [vp {:to}to {:infl :base :modal :none}study]]] |
| she will want to study |    [she      [vp {:infl :base   :modal :base}will           [vp {:infl :base :modal :infinitive}want   [vp{:to}to {:infl :base :modal :none}study]]]] |


## Syntax trees

With the following abbreviations:

- :infl(B=base,F=finite (e.g. :present))
- :modal(I=infinitive,N=none,T=to,B=base),

we have:

```
    s
   / \
she
       studies
       FN

       vp
      /  \
   will   study
   BI     BN

       vp
      /  \
   wants  vp
   FI    /  \
        to  study
        T   BN

       vp
    /      \
   will     vp
   B       /  \
         want  vp
         BI   /  \
             to  study
             T   BN
      vp
      / \
   will vp
   B   /  \
     want vp
     BI  /  \
        to  vp
         T  / \
          try vp
           BI / \
             to  study
              T  BN
```

Using the following abbreviations:

- FN: Finite inflection, Nonmodal
- FI: Finite inflection, Infinitive Modal
- T: to
- BN: Base inflection, Nonmodal
- BI: Base inflection, Infinitive Modal

- :infl(B=base,F=finite)
- :modal(I=infinitive,N=none,T=to,B=base),


The [:infl,:modal] possibilities shown in the above syntax trees are:

- [FN]
- [FI][T][BN]
- [B][BN]
- [B][BI][T][BN]
- [B][BI][T][BI][T][BN]

Or in regular expression form:

```
FN | ((FI T) | B) (BI T)* BN
```

## Generalizations

- a top-level VP's head is always one of: {FN,FI,B}.
- a top-level VP's comp depends on the choice of which of these is chosen.
- the top-level's VP comp and all below it depends on this choice.

## Lexical Rules

All verbs can be divided into one of these 3:

- B: Base Modals - will, would, could, etc
  - {:modal :base}
  - inflection
    - base inflection only
  - [:subcat :2]
     - [] (i.e. no :2]
     - BI
     - BN

- FI: Infinitive Modals: try, want, etc
  - {:modal :infinitive}
  - inflection
    - finite (FI)
    - base inflection (BI)
  - [:subcat :2]
     - [] (i.e. no :2)
     - T (i.e. to- phrases) e.g. "to study"

- N: nonmodals (study, sleep, etc)
  - {:modal :none}
  - inflection
    - finite (FN)
    - base inflection (BN)
  - transitivity:
    - intransitive (sleep, etc)
    - transitive (see, hit, etc)
  - [:subcat :2]
    - [] (i.e. no :2)
    - noun
