/*******************************************************************************/
/*@file cohort_enc_SCr.sql
/*
/*in: All_Scr_eGFR, AKI_Initial
/*       
/*out: AKI_Scr_eGFR
/*
/*action: write
/********************************************************************************/
create table AKI_Scr_eGFR as
with multi_match as (
select scr.*
from All_Scr_eGFR scr
where exists (select 1 from AKI_Initial aki where scr.ENCOUNTERID = aki.ENCOUNTERID and
              scr.LAB_ORDER_DATE between aki.ADMIT_DATE_TIME and aki.DISCHARGE_DATE_TIME)
union all
select aki.PATID
      ,aki.ENCOUNTERID
      ,scr.SERUM_CREAT
      ,scr.eGFR
      ,scr.LAB_ORDER_DATE
      ,scr.SPECIMEN_DATE_TIME
      ,scr.RESULT_DATE_TIME
from All_Scr_eGFR scr
join AKI_Initial aki
on scr.PATID = aki.PATID and scr.ENCOUNTERID <> aki.ENCOUNTERID and
   scr.LAB_ORDER_DATE between aki.ADMIT_DATE_TIME and aki.DISCHARGE_DATE_TIME
)   
   ,scr_cnt as (
select PATID
      ,ENCOUNTERID
      ,SERUM_CREAT
      ,eGFR
      ,LAB_ORDER_DATE
      ,SPECIMEN_DATE_TIME
      ,RESULT_DATE_TIME
      ,count(distinct SPECIMEN_DATE_TIME) over (partition by ENCOUNTERID) scr_tot
      ,dense_rank() over (partition by ENCOUNTERID order by LAB_ORDER_DATE,SPECIMEN_DATE_TIME) rn      
from multi_match
)
select distinct
       PATID
      ,ENCOUNTERID
      ,SERUM_CREAT
      ,eGFR
      ,LAB_ORDER_DATE
      ,SPECIMEN_DATE_TIME
      ,RESULT_DATE_TIME
      ,rn
from scr_cnt
where scr_tot > 1



