# DataDiggers
RProject for datadiggers

latest build status : [![Build Status](https://travis-ci.org/rlondt/DataDiggersRProject.svg?branch=master)](https://travis-ci.org/rlondt/DataDiggersRProject)

# Uitgangspunten
* Schaalbaar (qua ontwikkelcapaciteit)
  * versie beheer 
* fail fast / test driven development
  * Unitttest waarmogelijk
  * controleer input parameters
  * automatische bouw package
* herbruik waar mogelijk 
  * probeer de bouwblokjes zo klein mogelijk te houden
  * gebruik functies
  * caching van resultaten 

# Codeer standaarden
1. naamgeving van variabelen volgens lower camelCase (beginnend met kleine letter)
2. dataframe variabelen eindigen op DF
3. commentaar in het Nederlands
4. uitlijning middels twee spaties
5. voor zover mogelijk worden de packaged functies ge-unittest
6. alle herbruikbare functies worden naar de package verplaatst
7. alle analyse vindt plaats buiten de package
8. package-code wordt gedocumenteerd

