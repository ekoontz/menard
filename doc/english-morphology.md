# Introduction

Morphology is the means by which a surface string is converted its
canonical form, and the reverse: how a canonical form is converted to
a surface form. For example, noun morphology lets us turn the plural form
"dogs" into the canonical form "dog" and the reverse. Verb morphology
allows us to convert the gerund form "sleeping" into the canonical
form "sleep" as well as the reverse conversion.

## Notation

We'll use EDN as the format for morphological rules. For example, see
menard/english/nouns.edn. One such rule is:

```
{:g [#"(.*)$"   "$1s"]  ;; dog     -> dogs
 :p [#"(.*)s$"  "$1"]  ;;  dogs    -> dog
 :agr {:number :plur}}
```

All morphological rules are a Clojure map containing three keys:

- `:g`
- `:p`
- `:agr`


The value of `:g` (for "generate") is how to transform a
canonical form like "dog" into an inflected form like "dogs".

The value of `:p`: (for "parse") is the reverse: how to transform an
inflected form into a canonical form.

The value of `:agr` (for "agreement") encodes how the inflected form
differs from the canonical form. For generation, the agreement
information will be available in the input syntax tree, and this agreement
information will be input into the generative process to produce the
inflected form. For parsing, the agreement information will be unified
with the lexical form to produce the syntax tree as output.








