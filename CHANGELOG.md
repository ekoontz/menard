# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

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



