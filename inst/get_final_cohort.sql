/********************************************************************************/
/*@file get_final_cohort.sql
/*
/*in: AKI_stages_daily, AKI_Initial
/*
/*action: query
/********************************************************************************/
with pat_enc as (
select distinct 
       to_number(PATID) PATID
      ,to_number(ENCOUNTERID) ENCOUNTERID
      ,trunc(ADMIT_DATE_TIME) ADMIT_DATE
      ,SERUM_CREAT_BASE
from AKI_stages_daily
)
   ,onsets as (
select * from
(select ENCOUNTERID
       ,AKI_STAGE
       ,SPECIMEN_DATE
 from AKI_stages_daily
 where rn = 1)
pivot 
(min(SPECIMEN_DATE)
 for AKI_STAGE in (1 as AKI1_ONSET,
                   2 as AKI2_ONSET,
                   3 as AKI3_ONSET)
 )
 order by ENCOUNTERID
)
   ,onsets_val as (
select * from
(select ENCOUNTERID
       ,AKI_STAGE
       ,SERUM_CREAT
 from AKI_stages_daily
 where rn = 1)
pivot 
(max(SERUM_CREAT)
 for AKI_STAGE in (1 as AKI1_SCR,
                   2 as AKI2_SCR,
                   3 as AKI3_SCR)
 )
 order by ENCOUNTERID
)
   ,onsets_inc as (
select * from
(select ENCOUNTERID
       ,AKI_STAGE
       ,SERUM_CREAT_INC
 from AKI_stages_daily
 where RN = 1)
pivot 
(max(SERUM_CREAT_INC)
 for AKI_STAGE in (1 as AKI1_INC,
                   2 as AKI2_INC,
                   3 as AKI3_INC)
 )
 order by ENCOUNTERID
)
   ,raw_onset as (
select pe.PATID
      ,pe.ENCOUNTERID
      ,pe.ADMIT_DATE
      ,trunc(init.DISCHARGE_DATE_TIME) DISCHARGE_DATE
      ,pe.SERUM_CREAT_BASE
      ,ons.AKI1_ONSET
      ,(ons.AKI1_ONSET-pe.ADMIT_DATE) AKI1_SINCE_ADMIT
      ,scr.AKI1_SCR
      ,inc.AKI1_INC
      ,ons.AKI2_ONSET
      ,(ons.AKI2_ONSET-pe.ADMIT_DATE) AKI2_SINCE_ADMIT
      ,scr.AKI2_SCR
      ,inc.AKI2_INC
      ,ons.AKI3_ONSET
      ,(ons.AKI3_ONSET-pe.ADMIT_DATE) AKI3_SINCE_ADMIT
      ,scr.AKI3_SCR
      ,inc.AKI3_INC
from pat_enc pe
join AKI_Initial init
on pe.ENCOUNTERID = init.ENCOUNTERID
left join onsets ons
on pe.ENCOUNTERID = ons.ENCOUNTERID
left join onsets_val scr
on pe.ENCOUNTERID = scr.ENCOUNTERID
left join onsets_inc inc
on pe.ENCOUNTERID = inc.ENCOUNTERID
)
-- some pruning (recovering progress (e.g. AKI3 to AKI2) doesn't count)
select distinct
       PATID
      ,ENCOUNTERID
      ,ADMIT_DATE
      ,DISCHARGE_DATE
      ,SERUM_CREAT_BASE
      ,case when AKI1_SINCE_ADMIT >= AKI2_SINCE_ADMIT or AKI1_SINCE_ADMIT >= AKI3_SINCE_ADMIT then null
            else AKI1_ONSET end as AKI1_ONSET
      ,case when AKI1_SINCE_ADMIT >= AKI2_SINCE_ADMIT or AKI1_SINCE_ADMIT >= AKI3_SINCE_ADMIT then null
            else AKI1_SINCE_ADMIT end as AKI1_SINCE_ADMIT
      ,case when AKI1_SINCE_ADMIT >= AKI2_SINCE_ADMIT or AKI1_SINCE_ADMIT >= AKI3_SINCE_ADMIT then null 
            else AKI1_SCR end as AKI1_SCR
      ,case when AKI1_SINCE_ADMIT >= AKI2_SINCE_ADMIT or AKI1_SINCE_ADMIT >= AKI3_SINCE_ADMIT then null
            else AKI1_INC end as AKI1_INC
      ,case when AKI2_SINCE_ADMIT >= AKI3_SINCE_ADMIT then null 
            else AKI2_ONSET end as AKI2_ONSET
      ,case when AKI2_SINCE_ADMIT >= AKI3_SINCE_ADMIT then null 
            else AKI2_SINCE_ADMIT end as AKI2_SINCE_ADMIT
      ,case when AKI2_SINCE_ADMIT >= AKI3_SINCE_ADMIT then null 
            else AKI2_SCR end as AKI2_SCR
      ,case when AKI2_SINCE_ADMIT >= AKI3_SINCE_ADMIT then null 
            else AKI2_INC end as AKI2_INC
      ,AKI3_ONSET
      ,AKI3_SINCE_ADMIT
      ,AKI3_SCR
      ,AKI3_INC
from raw_onset
order by PATID, ENCOUNTERID;
