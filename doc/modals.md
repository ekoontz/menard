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

- :infl(B=base,P=present)
- :modal(I=infinitive,N=none,T=to,B=base),

we have:

```
    s
   / \
she
       studies
       PN

       vp
      /  \
   will   study
   BI     BN

       vp
      /  \
   wants  vp
   PI    /  \
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

And the [:infl,:modal] possibilities shown here are:

- [PN]
- [PI][T][BN]
- [B][BN]
- [B][BI][T][BN]
- [B][BI][T][BI][T][BN]

Or in regular expression form:

```
PN | ((PI T) | B) (BI T)* BN
```

## Generalizations

- a top-level VP's head is always one of: {PN,PI,B}.
- a top-level VP's comp depends on the choice of which of these is chosen.
- all VPs below this comp are also dependent on this choice.