External Validation for AKI Predictive Models using PCORnet CDM data
=====================================================================

This is the working directory building and validating predictive models for Acute Kidney Injury (AKI) based on PCORNET common data model across GPC sites.

by Xing Song, with Mei Liu, Russ Waitman
[Medical Informatics Division, Univeristy of Kansas Medical Center][MI]

[MI]: http://informatics.kumc.edu/

Copyright (c) 2018 Univeristy of Kansas Medical Center  
Share and Enjoy according to the terms of the MIT Open Source License.

***

## Background

Acute Kidney Injury (**AKI**) is a common and highly lethal health problem, affecting 10-15% of all hospitalized patients and >50% of the intensive care unit (ICU) patients. In this application, we propose to build predictive models to identify patients at risk for hospital-acquired AKI and externally validate the models using the PCORnet (Patient Centered Outcomes Research Network)13 common data model (CDM) infrastructure. The project will be carried out with the following aims:

* **Aim 1**: Building predictive models on single-site data. We will develop and internally cross-validate machine learning based predictive models for in-hospital AKI using electronic medical record (EMR) data from the University of Kansas Medical Center’s (KUMC) PCORnet CDM. As co-I of the PCORnet network Greater Plains Collaborative (GPC), PI of this project has direct access to the KUMC CDM for model development.
 
* **Aim 2**: Validating predictive models on multi-site data. We will implement an automated analytic package with built in data extraction and predictive modeling from Aim 1 for distributed execution within two PCORnet clinical data research networks (CDRNs), namely GPC led by Dr. Waitman and Veterans Health Administration (VHA) site led by Dr. Matheny in pSCANNER (letters attached). All prototyping will be done on the KUMC CDM.

***

## Site Usage
In order for sites to extract AKI cohort, run predictive models and generate final report, [R Program] is required and [R studio] is preferred to be installed as well for convenient report generation. Valid channel should also be established between R and DBMS so that communication between R and CDM database can be supported. 

[R Program]: https://www.r-project.org/
[R studio]: https://www.rstudio.com/

1. Get `AKI_CDM` code
  - download the [AKI_CDM] repository as a .zip file, unzip and save folder as `AKI_CDM`, *OR*
  - clone [AKI_CDM] repository (using [git command]): 
  
    a. nevigate to the location where you want to save the repository locally: `cd <local repo>` 
    
    b. clone the AKI_CDM repository: `git clone https://github.com/kumc-bmi/AKI_CDM` 


2. Prepare configeration file `config.csv` and save alongside the AKI_CDM project folder

    a. download the empty `config_template.csv` file
    
    b. fill in the content accordingly (or you can manually create the file using the following format):
    
    |username     |password    |access         |cdm_db_schema     |cdm_db_server         |oracle_temp_schema                            |
    |:------------|:-----------|:--------------|:-----------------|:---------------------|:---------------------------------------------|
    |your_username|your_passwd |//host:port/sid|current CDM schema|sid where CDM is saved|schema where intermediate tables will be saved|
    
    c. save as `config.csv` under the parent directory of AKI_CDM (so it would be at the same level as the AKI_CDM folder)

[AKI_CDM]: https://github.com/kumc-bmi/AKI_CDM
[git command]: https://git-scm.com/book/en/v2/Git-Basics-Getting-a-Git-Repository



3. Extract AKI cohort and generate final report
  - Knit `AKI_CDM_EXT_VALID_p1_QA.Rmd` and report the outputs 
  
    a. `AKI_CDM_EXT_VALID_p1_QA.html` 
    
    b. `AKI_CDM_EXT_VALID_p1_QA_TBL.xlsx` in the `data` folder 
    
  *OR*
  - Run `AKI_CDM_EXT_VALID_p1_QA.R` and report the output 
  
    a. `AKI_CDM_EXT_VALID_p1_QA_TBL.xlsx` in the `data` folder 


*Remark*: all the counts (patient, encounter, record) are masked as "<11" if the number is below 11

