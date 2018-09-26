/*******************************************************************************/
/*@file cohort_eligb.sql
/*
/*in: AKI_Scr_eGFR, AKI_Scr_base, exclude_all
/*
/*out: AKI_eligible
/*
/*action: write
/********************************************************************************/
create table AKI_eligible as
with exclud_unique as (
select distinct ENCOUNTERID
from exclude_all
)
--perform exclusion
  ,scr_all as (
select a.PATID
      ,a.ENCOUNTERID
      ,a.SERUM_CREAT
      ,a.EGFR
      ,a.SPECIMEN_DATE_TIME
      ,a.RESULT_DATE_TIME
      ,a.rn
from AKI_Scr_eGFR a
where not exists (select 1 from exclud_unique e
                  where e.ENCOUNTERID = a.ENCOUNTERID)
)
select scr.PATID
      ,scr.ENCOUNTERID
      ,scrb.ADMIT_DATE_TIME
      ,scrb.SERUM_CREAT SERUM_CREAT_BASE
      ,scrb.SPECIMEN_DATE_TIME SPECIMEN_DATE_TIME_BASE
      ,scrb.RESULT_DATE_TIME RESULT_DATE_TIME_BASE
      ,scr.SERUM_CREAT
      ,scr.EGFR
      ,scr.SPECIMEN_DATE_TIME
      ,scr.RESULT_DATE_TIME
      ,scr.rn
from scr_all scr
join AKI_Scr_base scrb
on scr.ENCOUNTERID = scrb.ENCOUNTERID
order by scr.PATID, scr.ENCOUNTERID, scr.rn

