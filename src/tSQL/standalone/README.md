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


- 1.1 Script walkthrough

      *  #AKI_Initial: include all inpatient visit ('EI','IP','IS') satisfying the following conditions: 
          
          * more than LOS>=2; AND 
          * between &&start_date and &&end_date; AND         
          * age at visits >= 18 years old
          
        \ 
        
      *  #All_Scr_eGFR: collected all serum creatinine records and their timestamps for all patients indentified in #AKI_Initial and calculated eGFR using MDRD formula
      
          * serum creatinine (SCr) is identified by LOINC codes: '2160-0','38483-4','14682-9','21232-4','35203-9','44784-7','59826-8'
          * two intermediate tables are created:       
              * Scr_all: collect all eligible SCr records from LAB_RESULT_CM table    
              
         \  
      
      *  #AKI_Scr_eGFR: merge multiple lab encounters in the same associated inpatient visit, and sort the labs within the same inpatient visit by date and time 
          
          * one intermediate tables is created: 
              * multi_match: we noticed the issue of multiple encounterid for different lab orders within the same inpatient visit. This table is to merge such lab encounters into the associated inpatient encounter
             
         \    
              
      *  #AKI_Scr_base: identify baseline SCr value for each patient-encounter which is either a) most recent SCr value 2-days prior to admission; or b) the first SCr value on the day of admission 
          * four intermediate tables are created: 
              * scr_enc1: get the first SCr value during the inpatient visit by taking advantage of the rank column ("rn") from #AKI_Scr_eGFR
              * scr_prior: collect all historical SCr values within 2 days prior to admission, if exists 
              * scr_prior1: if there exists multiple SCr values within 2 days prior to admission, get the latest one
              * scr_base_dup: combine two sources of baseline SCr. If someone has both a record prior to and at admission, use the prior one as the "real" baseline 

          \  

      *  #exclude_all: identify patient-encounters that satisfy the exclusion criteria.
          * seven intermediate tables are created:
              * AKI_EXCLD_1SCR_EN: identify patient-encounters with only 1 SCr record
              * AKI_EXCLD_L1GFR_EN: identify patient-encuonters with initial eGFR < 15
              * AKI_EXCLD_PRF_EN
              * AKI_EXCLD_PRF_EN: identify patient-encounters with pre-existing ESRD, or renal replacement therapy (RRT), or dialysis
              * scr48: an auxilary table, which calculates a time-window boundary for 48-hours since first SCr record
              * AKI_EXCLD_RT48_EN: identify patient-encounters who recieve RRT witin 48 hours since admission
              * AKI_EXCLD_BURN_EN: identiy patient-encounters who gets admitted due to burn
        
         \  
        
      *  #AKI_Eligible: finalize the eligible patient cohort by making all the exclusions and attach baseline SCr values
      
         \     
      
      *  #AKI_stages_daily: identify patients' AKI stages at each day during hospitalization
         * four intermediate tables are created: 
             * aki3_rrt: identify RRT during the stay (should be beyond 48-hr since first SCr record), which will be used to identify AKI stage 3
             * stage_aki: collect all pairwise SCr records (not necessarily adjacent) and identify AKI stage 1, 2 and 3 based on rolling baseline or static baseline
             * AKI_stages: rank all AKI stage incidences based on their timestamps
             * stage_uni: if there are multiple occurences of the same AKI stage, we will want to take the initial incidence ("onset" incidence)
             
        \       
       
       * #AKI_Onset: the final AKI_Onsets wide table with one patient-encounter per row
        * 
            * pat_enc: take out all distinct pair of patient, encounter
            * onsets: pivot over the date columns in the #AKI_stage_daily, so that we will have separate columns for AKI1_Onset, AKI2_Onset, AKI3_Onset, and NONAKI_ANCHOR
            * onsets_val: 
            * onsets_inc: 
            * raw_onset: merge columns from pat_enc, onsets, onsets_val, onsets_inc. However, for someone starting from AKI2 and recover to AKI1, we may still have kept both their AKI2 and AKI1 onset dates. In the final step, we will need to further clean up such cases by ignoring the recovering portion.  
      

       \      
       
       * #consort_diagram: collect counts from intermediate tables to create the consort diagram, which can help us to understand where does data attrition happen.This summary stats also gives an overall understanding of your local AKI cohort, in terms of cohort size and AKI incidence rates.


****************************************************************************

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
