# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

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



