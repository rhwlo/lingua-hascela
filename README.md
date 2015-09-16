# Lingua Hascela

**Lingua Hascela** defines a `Data.Latin` module for parsing and inflecting
Latin words. Currently it uses the `STEMFILE.GEN` and `INFLECTS.LAT` from
[Whitaker's Words][0], and I’m trying to add more functionality to it as I go.

Currently it can:
- conjugate verbs via `Data.Latin.Internal.conjugate`
- decline nouns via `Data.Latin.Internal.decline`
- parse [regular] nouns and verbs from Whitaker’s `STEMFILE.GEN`
- parse rules for declension and conjugation from Whitaker’s `INFLECTS.LAT`

In the future, it [hopefully] will:
- conjugate irregular verbs
- decline adjectives
- decline pronouns
- parse adjectives, adverbs, and pronouns from `STEMFILE.GEN`
- parse rules for declining adjectives and pronouns from `INFLECTS.LAT`

[0]: https://en.wikipedia.org/wiki/William_Whitaker's_Words
