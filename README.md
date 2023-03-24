# beemixtox

This is an R package for reproducing the resutlts of the manuscript for Bee Mixtox.

## Abstract

Understanding the frequency of non-additive effects of pesticides (synergism and antagonism) is important in the context of risk assessment. The goal of this study was to investigate the prevalence of non-additive effects of pesticides to honey bees ( Apis mellifera ). We investigated a large set of mixtures including insecticides and fungicides of different chemical modes of action and classes. The mixtures included represent a relevant sample of pesticides that are currently used globally. We investigated whether the experimental toxicity of the mixtures could be predicted based on the Concentration Addition (CA) model for acute contact and oral adult bee toxicity tests. We measured the degree of deviation from the additivity predictions of the experimental toxicity based on the well-known Mixture Deviation Ratio (MDR). Further, we investigated the appropriate MDR thresholds that should be used for the identification of non-additive effects based on acceptable rates for false positive (alpha) and true positive (beta) findings. We found that a deviation factor of MDR = 5 is a sound reference for labeling potential non-additive effects in acute adult bee experimental designs when assuming a typical Coefficient of Variation (CV%) = 100 in the determination of the LD 50 of a pesticide (a factor of 2x deviation in the LD 50 resulting from inter-experimental variability). We found that only a 2.4% and a 9% of the mixtures evaluated had an MDR > 5 and MDR < 0.2, respectively. The frequency and magnitude of deviation from additivity found for bees in this study are consistent with those of other terrestrial and aquatic taxa. Our findings suggest that additivity is a good baseline for predicting the toxicity of pesticide mixtures to bees, and that the rare cases of synergy of pesticide mixtures to bees are not random but have mechanistic basis.

## Data


## Reproduce the Results in the Manuscript

### Installing


```
install_github("Zhenglei-BCS/beemixtox_public")
```

### Data Selections and Curation

```
vignettes/articles/Ecotox-Database-Handling-and-Analysis.Rmd
```

```
vignettes/articles/OPP-Database-Handling-and-Analysis.Rmd
```

### Figures in the Manuscript

```
vignettes/articles/Figures-in-Manuscript.Rmd
```

### Supplemental Materials

```
vignettes/articles/Manuscript-Supplemental-Package.Rmd
```


## Authors


* Verena Tänzler; Arnd Weyers; Christian Maus; Markus Ebeling; Steven Levine; Ana Cabrera; Daniel Schmehl; Zhenglei Gao & Ismael Rodea-Palomares
* Maintainer: **Ismael M. Rodea Palomares** - [Ismael M. Rodea Palomares](mailto:ismaelm.rodeapalomares@bayer.com)
* Maintainer: **Zhenglei Gao** - [Zhenglei Gao](mailto:zhenglei.gao@bayer.com)

## Acknowledgments

* All Reviewers
