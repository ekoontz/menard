# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [1.11.2] - upcoming

Italian verb conjugation, and direct object pronouns in every language.

- Transitivity: "eat", "drink", "read", "play", "think", "do"/"make", "open"
  and "have" take a direct object but not a human one, and "dance" is
  intransitive (in every language that has the verb):
  - English: `:intransitive-only?` for "dance", and a new
    `:inanimate-object?` (with the matching
    `:subcat-frames-inanimate-object` rule) for "eat", "play", "think"
    (its plain sense; the clausal "think that ..." senses are untouched),
    "do" (likewise its lexical sense 1, not the do-support senses), "make",
    "open", "drink" (both its `:drink` and `:drink-t` senses), "read" and
    "have" sense 1 (not its perfect-auxiliary senses): their object carries
    `:ref {:human? false}`, so "I eat him", "I drink him", "I read him", "I
    play him", "I think him", "I do him", "I make him", "I open him" and
    "I have him" have no parse while "I eat a book", "I drink it", "I read
    it", "I play a book", "I think it", "I do it", "I make a book", "I open a
    book", "I have a book" and the object-free "I eat", "I drink", "I read",
    "I play", "I think", "I do", "I make", "I open" and "I have" still do.
  - Español: `:transitivity :intransitive` for "bailar", and two senses
    (intransitive, and transitive with `:obj {:ref {:human? false}}`) for
    "comer", "beber", "leer", "jugar", "tocar", "pensar", "abrir", "tener",
    the "drink" sense of "tomar" (whose "take" sense stays unconstrained) and
    each sense of "hacer" ("do" and "make"), so "yo lo como", "yo lo bebo",
    "yo lo leo", "yo lo toco", "yo lo pienso", "yo lo abro", "yo lo tengo"
    and "yo lo hago" only have the inanimate (`:pred :it`) reading.
  - Nederlands: `:inanimate-object?` plus a `:subcat-inanimate-object` rule
    for "eten", "spelen", "lezen" and "maken" (the lexicon has no "dansen",
    "denken", "doen" or "openen"). "hebben" is left unconstrained on purpose: the same
    verb is the one in "iets nodig hebben" (to need), whose object may be a
    person ("jij hebt hun nodig" = 'you need them'), so constraining it would
    take that reading away. "maken" now also lists its present singular forms
    (`maak`, `maakt`): like "spelen", its stem doubles the long vowel, which
    the morphology's `^(.*)en$` rule cannot derive, so "ik maak" and
    "hij maakt" had no parse before.
  - Italiano: `:subcat {:2 []}` for "ballare" (so "lo ballo" has no parse,
    like "li andiamo"), and `:sem {:obj {:human false}}` for "mangiare",
    "bere", "leggere", "giocare", "suonare", "pensare", "aprire", all three
    lexical senses of "avere" (the perfect-auxiliary senses are separate and
    untouched) and both senses of "fare" ("lo mangio", "lo bevo", "lo leggo",
    "lo gioco", "lo penso", "lo apro", "lo ho" and "lo faccio" keep only the
    inanimate reading of "lo"). New
    `menard.italiano.conjugate/takes-human-object?` tells a verb that can take
    a person as its direct object from one that cannot.

- Intransitive verbs: italiano "stare", english "stay", and "attend" in
  every language that has it (english "attend", español "asistir", italiano
  "frequentare") no longer take a direct object: "lo sto", "lo frequento",
  "yo lo asisto", "I attend a book" and "I stay him" have no parse, while
  "sto", "io sto", "frequento", "yo asisto", "I attend" and "I stay" still
  parse. Only the "stay" sense of "stare" is intransitive -- its two
  progressive-auxiliary senses keep their verb complement, so "sto
  camminando" still parses. Nederlands "blijven" already carried
  `:sem {:obj :none}`.

- Ditransitives: english "put" needs a nonhuman thing put and a nonhuman
  place: "I put the book on the table", "I put it on the table", "I put it
  in the house" and "I put it on it" parse, while "I put him on the table"
  (*the thing put is a person) and "I put it on her"/"I put it in her" (*the
  place is a person) have no ditransitive parse. Its `:put-on` and `:put-in`
  senses carry `:sem {:obj {:ref {:human? false}} :iobj {:ref {:human?
  false}}}`; the `:put-on-makeup` sense ("put makeup on someone") and the
  `:put-on` prepositional sense ("put on a coat") are left as they were.
  Español "poner" gets intransitive and transitive-with
  `:obj {:ref {:human? false}}` senses, and italiano "mettere" carries
  `:sem {:obj {:human false}}`; neither grammar has the PP complement, so
  there is no indirect object to constrain there, and the nederlands lexicon
  has no "put" verb ("zetten"/"leggen").

- Ditransitives: english "give" now needs a nonhuman direct object and a
  human recipient, which is what its meaning calls for: "she gives it to
  her"/"I give the book to her"/"I give it to him" parse, while "I give him
  to her" (*the thing given is a person) and "I give it to it" (*the
  recipient is a thing) have no ditransitive parse.
  - `resources/english/lexicon/verbs.edn`: give's ditransitive sense carries
    `:sem {:obj {:ref {:human? false}} :iobj {:ref {:human? true}}}`.
  - `resources/english/lexicon/misc.edn`: new `:recipient` sense of the
    preposition "to" whose complement must be a person, and the locative
    `:p1` sense now carries `:sem {:obj :none}`, which keeps it out of the
    ditransitive frame.
  - `resources/english/grammar.edn`: new `pp-recipient` rule, which copies
    the recipient's semantics into the phrase's `:sem :obj`
    (`menard.ug/comp-is-obj`) so that menard's `:ditransitive-verbs` rule can
    put it in the verb's `:sem :iobj`; without it the `:iobj` was left open.
    Recipient animacy is checked for pronouns ("her", "him", "them" vs "it");
    nouns in these lexicons mostly carry no `:sem :ref`, so a noun recipient
    is not distinguished ("I give it to the book" still parses).
  - Español "dar", italiano "dare" and nederlands (no "geven" lexeme) have no
    ditransitive or prepositional-complement construction in their grammars
    to constrain: neither "yo doy el libro a ella" nor "do il libro a lei"
    parses at all, and neither does any other PP complement ("yo voy a la
    tienda", "vado a la negozio"). Giving those languages the construction is
    a separate piece of work.

- Direct object pronouns, per language:
  - Italiano: add the direct object clitics `li` and `le`, which were
    missing, and non-reflexive senses for `mi`, `ti` and `ci` (only
    reflexive ones existed; `lo`, `la` and `vi` already had both). The
    `vp-pronoun-*` rules now carry the verb's semantics
    (`menard.ug/head-sem`) and put the clitic's meaning in `:sem :obj`
    (new `menard.ug/comp-is-obj`), so "lo vedo" and "il cane lo vede"
    mean 'I see him' instead of reading the clitic as the subject; the same
    rule makes an ordinary object noun phrase land in `:sem :obj`, which
    used to be left as `{:top :top}` ("il cane vede il gatto" lost the
    cat). The clause rule now requires a nominative noun subject and the
    vp rule an accusative noun object, and the non-reflexive clitic rules
    no longer demand `:phrasal? true`: menard's italian verbs are all
    `:phrasal? false`, so those rules never matched before.
  - Español: add non-reflexive direct object senses for `nos` and `os`,
    which only had reflexive ones, so "ella nos ve" and "yo os veo" parse
    ("él me ve", "yo te veo", "yo lo veo" and the other clitics already
    worked).
  - English and Nederlands already had their direct object pronouns; they
    are now covered by tests.
- Tests: new menard.test.english.direct-objects,
  menard.test.italiano.direct-objects and
  menard.test.nederlands.direct-objects, and a parsing test in
  menard.test.español.direct-objects.

- `menard.italiano.lexicon`: derive menard `:exceptions` from the
  babel-style `:italiano` irregular data in
  `resources/italiano/lexicon/verbs.edn`, so irregular verbs inflect
  correctly (vado, ho, sono, faccio, andrò, detto, ...).
- `menard.italiano.conjugate`: tonic and compound conjugation, with
  avere/essere auxiliary selection and reflexive clitics, plus
  `generate-chart` for the verbcoach conjugation table.
  `direct-object-clitic` and `with-direct-object-clitic` give the clitic
  direct object pronoun for an agreement (`lo`, `la`, `li`, `le`, and `mi`,
  `ti`, `ci`, `vi`) and prefix it to a verb form, eliding `lo`/`la` before a
  vowel ("lo vedo", "l'ho visto"); a test checks every clitic against
  italiano's non-reflexive accusative pronoun lexemes, and `object-less?`
  reports the verbs the lexicon marks as taking no direct object (`andare`,
  `arrivare`, which carry `:subcat {:2 []}`).
- `menard.italiano/generate-all`.
- Italiano grammar: share agreement between a noun and its adjective or
  determiner (`menard.ug/shared-agr`), and drop the `np-to-n-pp` rule, which
  let generation replace a requested noun phrase with a bare noun plus a
  random "di <noun>" complement.
- Italiano lexicon and `menard.translate.it-en`: add the formal `lei`
  pronoun (`:sem {:pred :you, :ref {:context :formal}}`) so the addressee
  reading survives translation: `it-subj->en-subj` keeps pronoun subjects
  instead of re-deriving them from `:agr` (which lost gender, turning
  "lei scrive" into "he writes"), and a formal `lei` subject puts the clause
  into english's second person so the verb takes its base form. "lei scrive"
  now translates as both "she writes" and "you <formal emoji> write".
- Italiano grammar: a modified noun can take a determiner again. The nbar
  rule used `menard.subcat/a-comp-with-one-subcat`, so the adjective consumed
  the noun's argument slot and "il cane nero" had no parse; it now uses
  `menard.subcat/b` and keeps the head's `:sem` (`menard.ug/head-sem`). The
  adjective lands in `:sem :mod :rest`, which leaves `:mod :first` to the
  determiner (new `menard.ug/sem-mod-rest-is-comp-sem`).
- Italiano grammar: port babel's `:phrasal false` as menard's `:phrasal?`, so
  an nbar's head must be a bare noun -- `[nbar1-woi-1 +[noun-phrase2 .il
  +cane] .affollato]` no longer parses -- and its complement a bare adjective.
- Italiano grammar: drop the duplicate `nbar1-woi-2`. babel had two copies of
  the noun+adjective rule differing only in the `:word-of-interest` child, a
  concept menard does not have, so the port's copies were identical and every
  such parse was reported twice.
- `menard.translate.it-en`: `it-det->en-quant` moves the noun's remaining
  modifiers into english's `{:first ... :rest ...}` modifier list, so a
  translated det+adjective noun phrase has english's shape.
- Italiano grammar: port babel's `:phrasal` constraints as menard's
  `:phrasal?` on the remaining rules (`s-aux`, `vp-aux-*`, `vp-32`,
  `vp-pronoun-*`, `s-modifier`, `noun-phrase1`, `noun-phrase2`). The port
  kept the key as `:phrasal`, which nothing sets, so every one of these
  constraints was inert -- for instance "ho parlato" could be headed by the
  phrasal-complement rule as well as the nonphrasal one, and `s-aux` could
  take a bare verb as its head.
- Italiano grammar: `noun-phrase1` (determiner + bare noun) builds again.
  Its `:sem ... :mod []` came from babel, where the determiner lands in
  `:sem :spec`; menard puts it in `:mod :first`, so the constraint failed
  against every determiner and the rule was dropped when the grammar loaded.
  It now splits the determiner+noun work with `noun-phrase2` by the head's
  phrasalness, as in babel: "il cane" is `noun-phrase1`, "il cane nero" is
  `noun-phrase2`.
- `menard.translate.it-en`: translate the clause parse (`it-clause-parse`)
  instead of the first parse. A sentence's words also parse as a bare verb or
  noun phrase, and translating one of those drops the rest of the sentence
  ("io parlo" was translated from its `vp-pronoun-nonphrasal` parse).
- Italiano second-person formality: `tu` and `voi` now carry
  `:sem :ref {:context :informal}`, and a pro-drop second-person subject
  (synthesized from `:agr`) defaults to informal. Without it english had no
  formality to go on, so it could pick any of its `you` entries and menard
  then appended an arbitrary formality emoji: "tu balli" came out as
  "you 👵 dance". It is now always a child emoji, and the formal `Lei` (which
  already carried `:context :formal`) always an older-person one.
  `it-subject->en-subject` also defaults any second person subject
  that arrives without a `:context` to `:informal` (a stale lexicon, or a spec
  built by a caller), so the annotation does not depend on the lexicon alone.
  Formality carried in `:agr` is honoured too: a spec built from a conjugation
  chart, which marks the polite second person as `:agr {:formal? true}` (the
  convention `menard.español.conjugate`'s `:2nd-formal` person uses), now gets
  the formal `you` and an older-person emoji, where before it was
  indistinguishable from an unmarked `tu`.

## [1.11.1] - 2026-10-19

Spanish morphology fixes.

- Nouns: the plural of -ón/-án drops the accent (canción -> canciones,
  afán -> afanes), and nouns ending in -y take -es (rey -> reyes).
- Adjectives: fix the masculine plural of adjectives ending in -ón/-án
  (holgazán -> holgazanes), in -z (feliz -> felices) and in -n; -n and -z
  are no longer handled by a single rule.
- Lexicon: split the common-noun rule by number, so a plural noun entry
  (e.g. a plural exception) gets its own subcat.

## [1.11.0] - 2026-10-14

Italian: port babel's Italian to menard, and translate Italian to English.

- Add resources/italiano and menard.italiano: grammar, morphology (nouns,
  determiners, adjectives and verbs) and lexicon, ported from babel.
- Add menard.translate.it_en: translate Italian sentences to English,
  including the subject as well as the verb, definite noun phrases,
  pronouns and possessive determiners.
- Add tests: menard.test.italiano, menard.test.italiano.morphology and
  menard.test.translate.it_en.
- Read .edn morphology files as EDN: replace regex literals with
  (re-pattern ...).
- Spanish: add plural rules for nouns ending in -z (lápiz -> lápices) and
  in a stressed -í or -ú (rubí -> rubíes, tabú -> tabúes), and add lápiz,
  vez, rubí and tabú to the lexicon.
- Fix a stray closing paren at the end of español/morphology/present.edn.
- Add .vscode/ to .gitignore.
- Upgrade to latest dag_unify release

## [1.10.7] - 2026-09-24

Fix accent for one Spanish verb: mentir

## [1.10.6] - 2026-09-10

Fix noun morphology for verbs ending in d,l,n and r.

## [1.10.5] - 2026-08-16

More Spanish verb fixes

## [1.10.4] - 2026-08-16

- Fix Spanish verb: hacer
- Replace all occurrances of {:informal? true} with {:formal? false}
- Agg irregulars for "venir"

## [1.10.3] - 2026-08-03

Fix Spanish verb: poner

## [1.10.2] - 2026-04-20

Fix Spanish verb: mentir

## [1.10.1] - 2026-03-17

Fix Spanish verb: venir

## [1.10.0] - 2026-03-10

Ensure that specific exceptions (e.g. 1st person singular present) to override more general stem-based exceptions e.g. seguir, conseguir.

## [1.9.25] - 2026-03-07

- Add some more Spanish and English verbs

## [1.9.24] - 2026-02-08

- Fix conjugation of "seguir" and "conseguir"

## [1.9.23] - 2026-02-01

- Fix accents with Spanish verbs
- Add irregular stem for one verb

## [1.9.22] - 2026-01-11

for performance, remove use of :syntax-tree for now during generation (#14)

* use plain dissoc rather than dag_unify.dissoc/dissoc-in

* remove :syntax-tree creation during generation

## [1.9.21] - 2025-12-07

- Improve lexical lookup when `:canonical` is provided as lookup's input.
- Add SECURITY.md

## [1.9.20] - 2025-12-07

- Factor out common code related to async conjugation

## [1.9.19] - 2025-12-07

- Support async rendering of conjugation table to improve responsiveness

## [1.9.18] - 2025-12-06

- Add correct exceptional conjugations for dormir and dormirse
- Modify conjugation table output to more closely resemble Wiktionary's output

## [1.9.17] - 2025-11-29

- Add [:sem :ref :human?] and [:sem :ref :gender] to Spanish pronouns

## [1.9.16] - 2025-11-26

- Fix bugs with handling :note-on-first-word so that e.g. Pedro y usted is translated as "You 👴🏻 and Pedro"
- Add tests for this fix

## [1.9.15] - 2025-11-15

- English verb morphology fixes
- Spanish verb fixes

## [1.9.14] - 2025-11-10

- English verb morphological fixes
- Partially rewrite Español commonnoun vs pronoun rules to better handle 'el' nouns like 'aqua'
- Add new menard.crossproduct namespace to reduce a sets of maps into a possibly smaller set where
  each member of the new set is the largest-possible non-fail unification of subsets of the members of the
  original set.

## [1.9.13] - 2025-10-30

-  Add 'busqué' (1st person singular preterito for buscar)

## [1.9.12] - 2025-10-20

- Fix 3rd persono plural of Spanish 'caer' and 'caerse'

## [1.9.11] - 2025-09-29

- Add more Spanish verbs and English equivalents
- Reduce generation max-depth from 15 to 6
- When looking up lexemes during generation with lexicon-index-fn, if none of the
  expected keys are supplied, return no lexemes rather than entire lexicon.

## [1.9.10] - 2025-09-15

- Encode [:formal :singular :masculine] and [:formal :singular :feminine] as emojis

## [1.9.9] - 2025-09-14

- add more Spanish verbs and English equivalents
- add game and music emoji sets to distinguish English "play" translations to Spanish (tocar vs jugar)

## [1.9.8] - 2025-09-08

- Add more Spanish verbs and fix conjugations of existing Spanish verbs

## [1.9.7] - 2025-09-07

- add "desayunar"/"have breakfast"
- add morphological rule fo English participles for "chat"->"chatting/chatted"
- Enable truncation for parsing Spanish to improve performance

## [1.9.6] - 2025-09-05

- Fix conjugation of verb "oír"

## [1.9.5] - 2025-09-05

- Fix preterito stem: "saber" -> "superion"
- Add irregular "abracé" for "abrazar"

## [1.9.4] - 2025-09-05

- Improve how reflexivity and transitivity are handled
- Add more Spanish and English verbs

## [1.9.3] - 2025-05-29

- Add ability to parse emojis into meanings (e.g. "you 🧓🏼👴🏽" means "formal 2nd person plural pronoun"
- Spanish irregular verb fixes

## [1.9.2] - 2025-04-21

- Spanish adjective morphology fixes.

## [1.9.0] - 2025-04-20

- Support custom language models for specific purposes.
- Support English constructions like "go to bed".

## [1.8.1] - 2025-03-25

- +abrir => to hug

## [1.8.0] - 2025-03-24

- Support Preterito Perfecto in Spanish.

## [1.7.12] - 2025-01-22

## [1.7.11] - 2025-01-13

- Fix boot stem for -ar verbs in 2nd and 3rd person singular.

## [1.7.10] - 2025-01-12

- Revert change made in 1.7.9 that set default [:sem :obj] to :none in Spanish to English translation

## [1.7.9] - 2025-01-12

- Fixes for Spanish verb conjugations

## [1.7.8] - 2024-09-29

- Add two Spanish verbs and English translations
- add (reload) convenience functions for Spanish and English

## [1.7.7] - 2024-08-15

- Add support for showing conjugations for all tenses in Spanish

## [1.7.6] - 2024-08-12

- Fix typo in reflexive pronoun: "vos" should be "os"
- Restrict meaning of "querer": should not be "love" or "like"
- Restrict subject of "comer" to human only

## [1.7.5] - 2024-08-10

- Add missing annotation :masculine to "you" (sense 2)
- Add missing :sense to all "you" senses
- Add "Pedro y tú/usted" variants
- Add gender and notes for "they" to distinguish ellas/ellos
- Support :note-on-first-word to add emojis after the first word to indicate gender or formality

## [1.7.4] - 2024-08-10

- Add annotations to distinguish "nosotras" and "nosotras"
- Add [:canonical]=[:root] equivalence for all three languages

## [1.7.3] - 2024-08-08

- Fix reflexive agreement (e.g. prevent "he sees itself")

## [1.7.2] - 2024-08-07

- Add missing :extensions for one sense of put (en)
- :prepositional-verb? true => :reflexive false (en)

## [1.7.1] - 2024-08-06

- Fix 2nd person informal pronoun in Spanish
- Performance optimizations

## [1.7.0] - 2024-08-05

- Add first Spanish support
- Improve diagnostics and `developer-mode` support in generation, parsing and lexicon compilation
- New emoji icons to distinguish formal and informal second person pronouns.

## [1.6.4] - 2022-11-19

- Improve word-grouping algorithm and move to new namespace: 'menard.parse.word'.
- Replace dynamic variables with function parameters.
- Fixes for Dutch noun pluralization morphology.
- Move English tenses to their own namespace: 'menard.english.tenses'.

## [1.6.3] - 2022-09-25

- Add three keys to every model: :lexicon-fn, :morph-fn, :syntax-tree-fn to simplify calling of these functions with the correct model.
- Use known-working version of GraalVM's native-image in Dockerfile for creating AWS Lambda native image.
- Improvements for woordenlijst models.

## [1.6.2] - 2022-09-13

- handlers.clj: most generation and parse functions require a 'model' param.
- server/ definition updates to support 'model' param.
- AWS Lambda definition updates to support 'model' param.
- Logging improvements.
- Remove previously-deprecated AWS deflambda Generate
- Remove previously-deprecated server.clj's route /generate.
- Add some woordenlijst-related unit tests.

## [1.6.1] - 2022-09-10

- Multi-word tokenization integrated with menard.parse/parse-start.
- Whitespace code formatting cleanup.

## [1.6.0] - 2022-09-04

### Core

- Support multi-word tokenization in menard.parse/parse
- Introduce new model .edn format with :name, :language, :morphology, :grammar :lexicon and
  models in this format for nl and en.
- Update a few dependencies.
- Remove use of cljslog; use new built-in menard.log instead
- Add new woordenlijst language model and lexicon in nl and en and intermediate source files.
- Move tenses from menard.nederlands to their own namespace menard.nederlands.tenses.

### Server

- Update a few dependencies.
- Improve startup script: add MENARD_DIR where needed and add comments.

## [1.5.0] - 2022-03-13

- Fix/refactor reflexive pronouns.
- Allow for reloading of models based on polling filesystem changes using babashka/fs.
- Incremental parsing so that partial parses can be shared between clients and servers.
- All keys taking only true/false are suffixed with '?'
- Move all linguistic structures (e.g. ug, subcat) to .edn files in resources.

## [1.4.5] - 2021-08-7

- Fix typo

## [1.4.4] - 2021-08-7

- More work on simple past verbs
- Refactor server-side support to support both ring and AWS Lambda

## [1.4.3] - 2021-06-16

- Rewrite support for simple past verbs in Dutch.

## [1.4.2] - 2021-06-05

- Rewrite lexical rules to unify all rules into a single file per language.

## [1.4.1] - 2021-05-11

- Add lambda directory for deploying API to Amazon Lambda.
- Improve support for reflexive pronouns.
- Add more vocabulary.

## [1.4.0] - 2021-01-07

- Move linguistic resources out of `src/` and into `resources/`.

## [1.3.5] - 2020-11-14

- Move all linguistic resources to resources/, to better emphasize that they 
are data, not executable code.

## [1.3.4] - 2020-11-7

- Fix synchronization of model-loading, so a model is only added once
rather than once per-thread, if the menard libary is used in a
multi-threaded application or service.

## [1.3.3] - 2020-09-13

- Upgrade to latest dag_unify release.
- Fix and refactor Dutch adjective morphology.

## [1.3.2] - 2020-09-12

- Upgrade to latest dag_unify release.
- Add morphology for `dik` <-> `dikke`.

## [1.3.1] - 2020-08-15

- Improve loading and reloading language models
  - block on model-creation if it doesn't exist yet.
  - only load model once unless doing reloading.
- Apostrophe-s pluralization applies to all Dutch
  nouns that end in 'y' *or* vowels (not just 'y'),
  but 'vowels' doesn't include 'e' for the purpose of
  this rule.

## [1.3.0] - ??
- Release tag and commit for this release missed somehow.

## [1.2.1] - 2020-07-26

- Fix bugs in Dutch morphology of adjectives and nouns
- Add new lexemes
- Separate intensifiers and exclamations into their own files for readability
- Improve code readability with variable name changes and value usages in 
  menard/generate
- Improve readability of noun lexemes in Dutch by defaulting gender to :common
  if not specified

## [1.2.0] - 2020-07-14

- Fix bugs related to dynamic model-loading
- Turn down logging

## [1.1.0] - 2020-07-12

- Add new dynamic model-loading, so that a java process can update its linguistic
state from the filesystem, without restarting the whole JVM to get an updated
linguistic model.

## [1.0.3] - 2020-07-09

- Fix noun pluralization errors in Dutch (e.g "been", "fiets", "plaats").

## [1.0.2] - 2020-07-07

- Prevent overmatching on default pluralization parsing rule in Dutch.

## [1.0.1] - 2020-07-07

- Reduce redunancy in lexical source code files
- Add more lexical entries

## [1.0.0] - 2020-06-30

- Dutch adjective morphology fixes:
 - eenzaam -> eenzame
 - constrain m -> mm (e.g. 'stom' -> 'stomme') : 'm' must be preceded by 't'

## [0.0.4] - 2020-06-29

- ~analyze: create lower,upper, and capitalized variants of all inputs
- ~parse: use (newly-modified) analyze for parsing's lookup function
- fix for one sense of irregular plural nouns: :quant should be :some, not :unspec
- all possessive pronouns need to have both :sing and :plur senses
- cleanup whitespace
- move demo output to separate file

## [0.0.3] - 2020-06-21

- Rename from 'babylon' to 'menard'
- Update README.md with information about new name

## [0.0.2] - 2020-06-20

- Add some new lexemes.
- Add :example key to nederlands/expressions.edn where appropriate.
- Upgrade to latest dag_unify version available.
- Add CHANGELOG.md

## [0.0.1] - 2020-06-09

First release.



