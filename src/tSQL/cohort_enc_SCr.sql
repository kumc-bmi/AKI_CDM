/*******************************************************************************/
/*@file cohort_enc_SCr.sql
/*
/*in: #All_Scr_eGFR, #AKI_Initial
/*       
/*out: #AKI_Scr_eGFR
/*
/*action: write
/********************************************************************************/
with multi_match as (
select scr.*
from #All_Scr_eGFR scr
where exists (select 1 from #AKI_Initial aki where scr.ENCOUNTERID = aki.ENCOUNTERID)
union all
select aki.PATID
      ,aki.ENCOUNTERID
      ,scr.SERUM_CREAT
      ,scr.eGFR
      ,scr.LAB_ORDER_DATE
      ,scr.SPECIMEN_DATE_TIME
      ,scr.RESULT_DATE_TIME
from #All_Scr_eGFR scr
join #AKI_Initial aki
on scr.PATID = aki.PATID and scr.ENCOUNTERID <> aki.ENCOUNTERID and
   scr.LAB_ORDER_DATE between aki.ADMIT_DATE_TIME and aki.DISCHARGE_DATE_TIME
)
select distinct
       mm.PATID
      ,mm.ENCOUNTERID
      ,mm.SERUM_CREAT
      ,mm.eGFR
      ,mm.LAB_ORDER_DATE
      ,mm.SPECIMEN_DATE_TIME
      ,mm.RESULT_DATE_TIME
      ,dense_rank() over (partition by mm.ENCOUNTERID order by mm.LAB_ORDER_DATE,mm.SPECIMEN_DATE_TIME) rn      
into #AKI_Scr_eGFR
from multi_match mm



