# MiMe

This repository has the pipelines used in [] for investgating longitudinal trajectories in fecal microbiome and metabolome during the first 12 months of life from 99 infants.

## Menu
 1. [Data source](#data_source)
 2. [Data analysis modules](#data_analysis)

<a name="data_source"></a>
### 1) Data source

The microbiome and metabolome data can be downloaded from the repoisotry
  - **[Microbiome data ()]()**
  - **[Metabolome data ()]()**

<a name="data_analysis"></a>
### 2) Data analysis modules

...

The workflow used for analysis can be downloaded from the following [link](https://github.com/QuantGen/MAIZE-HUB/blob/main/analysis.zip).


```
pipeline_analysis
├── 1_data_processing
│   ├── code
│   │   ├── 1_auc_meta.R
│   │   ├── 1_auc_micro.R
│   │   ├── 2_filter_normalization.R
│   │   ├── 3_make_Figure_1_d-e.R
│   │   ├── 4_make_Figure_1_f-g.R
│   │   └── 5_make_Figure_S1.R
│   └── output
├── 2_model
│   ├── code
│   │   ├── 1_model_meta.R
│   │   ├── 1_model_micro.R
│   │   ├── 2_make_Figure_2_a-d.R
│   │   ├── 3_make_Figure_2_e-f.R
│   │   ├── 4_make_Figure_S2.R
│   │   └── 5_make_Figure_S3.R
│   └── output
├── 3_correlation
│   ├── code
│   │   └── 1_bootstrap_unbiased_cor.R
│   └── output
├── 4_enrichment
│   ├── code
│   │   ├── 1_enrich_meta.R
│   │   ├── 1_enrich_micro.R
│   │   └── 2_make_Figure_4_a.R
│   └── output
└── README

```
