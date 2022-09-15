# Create bilingual .edn file from CSV

```
cat Woordenlijst_CODE\ +deel\ 1_Absolute\ Beginners_N1_met\ Arabisch_zomer22.csv | ./csv2edn.sh > /Users/ekoontz/menard/woordenlijst-nouns.edn
```

# Create monolingual .edn files from bilingual .edn file

```
menard.nederlands> (load "woordenlijst")
menard.nederlands> (write-woordenlijst)


```

This will create:

- /Users/ekoontz/menard/resources/nederlands/lexicon/woordenlijst/nouns.edn
- /Users/ekoontz/menard/resources/english/lexicon/woordenlijst/nouns.edn

