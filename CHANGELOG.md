# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

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



