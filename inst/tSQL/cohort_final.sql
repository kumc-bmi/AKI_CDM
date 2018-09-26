/********************************************************************************/
/*@file cohort_final.sql
/*
/*in: #AKI_stages_daily, #AKI_Initial
/*
/*out: #AKI_onsets
/*
/*action: write
/********************************************************************************/
create table AKI_onsets as
with pat_enc as (
select distinct 
       to_number(PATID) PATID
      ,to_number(ENCOUNTERID) ENCOUNTERID
      ,trunc(ADMIT_DATE_TIME) ADMIT_DATE
      ,SERUM_CREAT_BASE
from #AKI_stages_daily
)
   ,onsets as (
select ENCOUNTERID, [0] as NONAKI_ANCHOR, [1] as AKI1_ONSET, [2] as AKI2_ONSET, [3] as AKI3_ONSET from
(select ENCOUNTERID
       ,AKI_STAGE
       ,SPECIMEN_DATE
 from #AKI_stages_daily
 where rn_asc = 1 and AKI_STAGE_max > 0 and AKI_STAGE > 0
 union all
 select ENCOUNTERID
       ,AKI_STAGE
       ,SPECIMEN_DATE
 from #AKI_stages_daily
 where rn_desc = 1 and AKI_STAGE_max = 0 and AKI_STAGE = 0
 )as s1
pivot 
(min(SPECIMEN_DATE)
 for AKI_STAGE in ([0] ,
                   [1] ,
                   [2] ,
                   [3] )
 ) as pvt1
)
   ,onsets_val as (
select ENCOUNTERID, [0] as NON_AKI_SCR, [1] as AKI1_SCR, [2] as AKI2_SCR, [3] as AKI3_SCR from
(select ENCOUNTERID
       ,AKI_STAGE
       ,SERUM_CREAT
 from #AKI_stages_daily
 where rn_asc = 1 and AKI_STAGE_max > 0 and AKI_STAGE > 0
 union all
 select ENCOUNTERID
       ,AKI_STAGE
       ,SERUM_CREAT
 from #AKI_stages_daily
 where rn_desc = 1 and AKI_STAGE_max = 0 and AKI_STAGE = 0) as s2
pivot 
(max(SERUM_CREAT)
 for AKI_STAGE in ([0] ,
                   [1] ,
                   [2] ,
                   [3] )
 ) as pvt2
)
   ,onsets_inc as (
select ENCOUNTERID, [0] as NON_AKI_INC, [1] as AKI1_INC, [2] as AKI2_INC, [3] as AKI3_INC from
(select ENCOUNTERID
       ,AKI_STAGE
       ,SERUM_CREAT_INC
 from #AKI_stages_daily
 where rn_asc = 1 and AKI_STAGE_max > 0 and AKI_STAGE > 0
 union all
 select ENCOUNTERID
       ,AKI_STAGE
       ,SERUM_CREAT_INC
 from #AKI_stages_daily
 where rn_desc = 1 and AKI_STAGE_max = 0 and AKI_STAGE = 0) as s3
pivot 
(max(SERUM_CREAT_INC)
 for AKI_STAGE in ([0],
                   [1],
                   [2],
                   [3])
 ) as pvt3
)
   ,raw_onset as (
select pe.PATID
      ,pe.ENCOUNTERID
      ,pe.ADMIT_DATE
      ,trunc(init.DISCHARGE_DATE_TIME) DISCHARGE_DATE
      ,pe.SERUM_CREAT_BASE
      ,ons.NONAKI_ANCHOR
      ,(ons.NONAKI_ANCHOR-pe.ADMIT_DATE) NONAKI_SINCE_ADMIT
      ,NON_AKI_SCR
      ,NON_AKI_INC
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
join #AKI_Initial init
on pe.ENCOUNTERID = init.ENCOUNTERID
left join onsets ons
on pe.ENCOUNTERID = ons.ENCOUNTERID
left join onsets_val scr
on pe.ENCOUNTERID = scr.ENCOUNTERID
left join onsets_inc inc
on pe.ENCOUNTERID = inc.ENCOUNTERID
)
-- some pruning (recovering progress doesn't count)
select distinct
       PATID
      ,ENCOUNTERID
      ,ADMIT_DATE
      ,DISCHARGE_DATE
      ,SERUM_CREAT_BASE
      ,NONAKI_ANCHOR
      ,NONAKI_SINCE_ADMIT
      ,NON_AKI_SCR
      ,NON_AKI_INC
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
into #AKI_onsets
from raw_onset
order by PATID, ENCOUNTERID

