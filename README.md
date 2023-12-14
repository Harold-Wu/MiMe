# MiMe (to be changed)

This repository has the pipelines used in **Population and subject-specific trajectories of fecal microbes and metabolites through early childhood** for investgating longitudinal trajectories in fecal microbiome and metabolome during the first 12 months of life from 99 infants.

If you have questions about the content of this repository, please contact Hao Wu (wuhao12@msu.edu) and Douglas Guzior (guziordo@msu.edu).

## Menu
 1. [Data source](#data_source)
 2. [Data analysis modules](#data_analysis)

<a name="data_source"></a>
### 1) Data source

Raw microbiome and metabolome data
  - Mass spectrometry data is available on MassIVE under ID MSV000092782. LC-MS/MS spectral annotation and molecular networking by GNPS are available at [link](gnps.ucsd.edu/ProteoSAFe/status.jsp?task=7454748a6baa406b909540b1c90a4e7e).
  - 16S amplicon data is available on Qiita under study ID 14092 with subsequent analysis and taxonomy assignment available under analysis ID 48437.

R-readable microbiome, metabolome, and their taxonomy data sets can be downloaded from [Figshare](https://figshare.com/articles/dataset/Dataset/24807804). Data sets are organized in the following structure:

```
Data
├── meta_data.RData
├── micro_data.RData
├── meta_taxa.RData
└── micro_taxa.RData
```

You can read microbiome and metabolome data sets into a R environment using the following code:

```r
load('meta_data.RData')
load('micro_data.RData')
```


<a name="data_analysis"></a>
### 2) Data analysis modules

The workflow used for analysis can be downloaded from the [link](https://github.com/Harold-Wu/MiMe/blob/main/Pipeline.zip). 

Once the file is uncompressed, it will generate a collection of folders, each containing a module of the workflow (see folder tree below). The number that preceeds the folder name indicate the order in which the modules need to be run. Because some modules use outputs from other modules as inputs, as a rule of thumb we recommend running the modules sequentially.

```
Pipeline
├── 1_data_processing
│   ├── code
│   │   ├── 1_data_QC.R
│   │   ├── 2_auc_meta.R
│   │   ├── 2_auc_micro.R
│   │   ├── 3_filter_normalization.R
│   │   ├── 4_make_Figure_1_a-b.R
│   │   ├── 5_make_Figure_1_c-d.R
│   │   └── 6_make_Figure_S1.R
│   └── output
├── 2_model
│   ├── code
│   │   ├── 1_model_meta.R
│   │   ├── 1_model_micro.R
│   │   ├── 2_make_Figure_2_a-d.R
│   │   ├── 3_make_Figure_3_a-b.R
│   │   ├── 4_make_Figure_S2.R
│   │   └── 5_make_Figure_S3.R
│   └── output
├── 3_correlation
│   ├── code
│   │   ├── 1_bootstrap_unbiased_cor.R
│   │   └── 2_make_Figure_S4.R
│   └── output
├── 4_enrichment
│   ├── code
│   │   ├── 1_enrich_meta.R
│   │   ├── 1_enrich_micro.R
│   │   └── 2_make_Figure_4_a.R
│   └── output
└── README
```
