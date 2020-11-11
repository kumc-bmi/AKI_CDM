Standalone tSQL scripts for local data extraction out of PCORnet CDM
====================================================================


by Xing Song, with Mei Liu, Russ Waitman
[Medical Informatics Division, Univeristy of Kansas Medical Center][MI]

[MI]: http://informatics.kumc.edu/

Copyright (c) 2018 Univeristy of Kansas Medical Center  
Share and Enjoy according to the terms of the MIT Open Source License.

***

## Site Usage 

1. `AKI_extract_cohort.sql` is used to extract the AKI cohort that satisfies the [inclusion and exclusion criteria]. The following two output tables are expected to be delivered:        
      * AKI_onsets
      * consort_diagram

[inclusion and exclusion criteria]: https://github.com/kumc-bmi/AKI_CDM/blob/master/report/AKI_CDM_EXT_VALID_p1_QA.Rmd

2. `AKI_collect_data.sql` is used to collect all relavent clinical observations for the AKI cohort against local PCORnet CDM schema.The following two output tables are expected to be delivered:           

      * AKI_DEMO
      * AKI_VITAL
      * AKI_PX
      * AKI_DX
      * AKI_LAB
      * AKI_PMED (skip it, if Prescribing table is not populated)
      * AKI_AMED (skip it, if Med_Admin table is not populated)
      * AKI_DMED (skip it, if Dispensing table is not populated)

Remark: but make sure to extract and deliver at least one table with inpatient medication information
