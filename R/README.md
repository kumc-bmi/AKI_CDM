Development and Validation Pipeline for AKI Predictive Models using PCORnet CDM data
========================================================================================

This is the working directory building and validating predictive models for Acute Kidney Injury (AKI) based on PCORNET common data model.

by Xing Song, with Mei Liu, Russ Waitman
[Medical Informatics Division, Univeristy of Kansas Medical Center][MI]

[MI]: http://informatics.kumc.edu/

Copyright (c) 2018 Univeristy of Kansas Medical Center  
Share and Enjoy according to the terms of the MIT Open Source License.

***

## Data Extraction 

`data_extract.R`
`meta_collect.R`

## Data Preprocssing

`data_preproc.R`


## Build DS-GBT model

`DSGBT_BayesOpt.R`


## Build DS-LASSO model

`DSGLM_Simple.R`


## Post-Analysis I: Model Evaluation

`model_eval.R`


## Post-Analysis II: Model Interpretation

`model_explain.R`


## Post-Analysis III: Distribution Analaysis

`distribution_analysis.R`



