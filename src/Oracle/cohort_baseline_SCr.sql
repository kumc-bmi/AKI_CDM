/*******************************************************************************/
/*@file cohort_baseline_SCr.sql
/*
/*in: AKI_Initial, AKI_Scr_eGFR, All_Scr_eGFR
/*       
/*out: AKI_Scr_base
/*
/*action: write
/********************************************************************************/
create table AKI_Scr_base as
--get first record within the encounter (baseline)
with scr_enc1 as (
select scr.* from AKI_Scr_eGFR scr
where scr.rn = 1
)
--looks like there exists multiple historical values on the same day 
select scrb.PATID
      ,scrb.ENCOUNTERID
      ,init.ADMIT_DATE_TIME
      ,init.DISCHARGE_DATE_TIME
      ,min(scrb.SERUM_CREAT) SERUM_CREAT
      ,max(scrb.eGFR) eGFR
      ,scrb.LAB_ORDER_DATE
      ,scrb.SPECIMEN_DATE_TIME
      ,scrb.RESULT_DATE_TIME
from scr_enc1 scrb
join AKI_Initial init
on scrb.ENCOUNTERID = init.ENCOUNTERID
group by scrb.PATID,scrb.ENCOUNTERID,init.ADMIT_DATE_TIME,scrb.LAB_ORDER_DATE,scrb.SPECIMEN_DATE_TIME,scrb.RESULT_DATE_TIME

