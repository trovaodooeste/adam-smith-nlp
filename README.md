# Replication: Text as Data and the Adam Smith Problem

**Paper:** "The Adam Smith Problem Revisited: Sentiment Analysis with the Loughran-McDonald Dictionary and Topic Modeling in *The Theory of Moral Sentiments* and *The Wealth of Nations*"

**Authors:** Wellington Gubiani Maiberg (ORCID: 0009-0002-7236-4367) · Ikaro R. M. de Oliveira (ORCID: 0009-0006-9940-4532)

**Submitted to:** *Journal of the History of Economic Thought* (Cambridge University Press)

---

## Overview

This repository contains all replication materials for the paper. We apply the Loughran-McDonald (LM) financial dictionary and Latent Dirichlet Allocation (LDA) to Adam Smith's *Theory of Moral Sentiments* (1759) and *Wealth of Nations* (1776), addressing the methodological limitation identified by Ballandonne & Cersosimo (2023, JHET), who found generic sentiment dictionaries inadequate for historical economic texts.

---

## Repository structure

```
├── scripts/
│   ├── precache_lm.R          # Pre-caches the LM dictionary (run once)
│   ├── 01_baixar_textos.R     # Downloads texts from Project Gutenberg
│   ├── 02_nlp_textos.R        # Full NLP pipeline (LM scores, LDA, readability)
│   ├── 03_analise_opcaoA.R    # Main analysis: Adam Smith Problem hypotheses
│   └── 04_figuras.R           # Generates all figures
├── resultados/
│   ├── nlp_textos.csv         # NLP variables by chapter and text
│   └── opcaoA_tfidf_smith.csv # TF-IDF results (TMS vs. WN)
├── figuras/
│   ├── fig1_lm_perfil.png     # LM sentiment profiles
│   ├── fig2_pronomes.png      # Personal pronouns
│   ├── fig3_uncertainty_boxplot.png
│   ├── fig4_lda_topicos.png   # LDA topic proportions
│   ├── fig5_tfidf_lollipop.png
│   └── fig6_legibilidade.png  # Readability indices
├── textos/
│   ├── smith_tms.txt          # Theory of Moral Sentiments (Gutenberg ID 67363)
│   └── smith_won.txt          # Wealth of Nations (Gutenberg ID 3300)
└── referencias.bib
```

---

## How to replicate

### Requirements

- R 4.2.3 or later
- Packages: `gutenbergr`, `tidyverse`, `tidytext`, `textdata`, `topicmodels`, `quanteda`, `quanteda.textstats`, `slam`

Install all at once:

```r
install.packages(c(
  "gutenbergr", "tidyverse", "tidytext", "textdata",
  "topicmodels", "quanteda", "quanteda.textstats", "slam"
))
```

### Execution order

```r
# Step 1: cache LM dictionary (run once; ~30 seconds, requires internet)
source("scripts/precache_lm.R")

# Step 2: download texts from Project Gutenberg
source("scripts/01_baixar_textos.R")

# Step 3: NLP pipeline
source("scripts/02_nlp_textos.R")
# Output: resultados/nlp_textos.csv

# Step 4: analysis and hypothesis testing
source("scripts/03_analise_opcaoA.R")
# Output: resultados/opcaoA_tfidf_smith.csv

# Step 5: figures
source("scripts/04_figuras.R")
# Output: figuras/fig1–fig6
```

---

## Data sources

- *Theory of Moral Sentiments* (6th ed., 1790): Project Gutenberg ID 67363
- *Wealth of Nations* (1776): Project Gutenberg ID 3300
- Loughran-McDonald dictionary: via R package `textdata` (Loughran & McDonald 2011, *Journal of Finance*)

---

## Reference

Ballandonne, M. & Cersosimo, I. (2023). Towards a 'Text as Data' Approach in the History of Economics: An Application to Adam Smith's Classics. *Journal of the History of Economic Thought*, 45(1), 27–49. https://doi.org/10.1017/S1053837222000104
