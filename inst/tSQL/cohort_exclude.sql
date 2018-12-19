/*******************************************************************************/
/*@file cohort_exclude.sql
/*
/*in: #AKI_Scr_eGFR, #AKI_Initial
/*
/*params: &&dbname, &&PCORNET_CDM
/*
/*out: #exclude_all
/*
/*action: write
/********************************************************************************/
-- Only one Scr record at encounter
with AKI_EXCLD_1SCR_EN as (
select ENCOUNTERID, max(rn) lab_cnt 
from #AKI_Scr_eGFR
group by ENCOUNTERID
having max(rn) <= 1
)
-- At CKD stage 4 or higher
    ,AKI_EXCLD_L1GFR_EN as (
select distinct ENCOUNTERID
from #AKI_Scr_eGFR
where rn = 1 and eGFR <= 15
)
-- Pre-existing renal failure (DX)
    ,AKI_EXCLD_PRF_EN as (
select aki.ENCOUNTERID
from #AKI_Initial aki
where exists (select 1 from [&&dbname].[&&PCORNET_CDM].DIAGNOSIS dx
              where dx.PATID = aki.PATID and
                    -- ICD9 for renal failure
                    ((dx.DX_TYPE = '09' and
                      (   dx.DX like '586%'
                       or dx.DX like '585.9%')
                      ) or
                    -- ICD10 for renal failure
                     (dx.DX_TYPE = '10' and
                      (   dx.DX like 'N18%'
                       or dx.DX like 'N19%')
                       )
                      ) and
                    dx.ADMIT_DATE < CONVERT(date, aki.ADMIT_DATE_TIME)
                )
)
-- Receive renal transplant withing 48 hr since 1st Scr (PX, DX)
    ,scr48 as (
select PATID, ENCOUNTERID,
       dateadd(day,2,SPECIMEN_DATE_TIME) time_bd
from #AKI_Scr_eGFR
where rn = 1
)
    ,AKI_EXCLD_RT48_EN as (
select distinct scr48.ENCOUNTERID
from scr48
where exists (select 1 from [&&dbname].[&&PCORNET_CDM].DIAGNOSIS dx
              where dx.PATID = scr48.PATID and
                    -- ICD9 for RRT
                    ((dx.DX_TYPE = '09' and
                      (   dx.DX like '996.81%'
                       or dx.DX like 'V42.0%')
                      ) or
                    -- ICD10 for RRT
                     (dx.DX_TYPE = '10' and
                      (   dx.DX like 'Z94.0%'
                       or dx.DX in ('T86.10', 'T86.11', 'T86.12'))
                       )
                      ) and
                    dx.ADMIT_DATE < scr48.time_bd
                )
union
select distinct scr48.ENCOUNTERID
from scr48
where exists (select 1 from [&&dbname].[&&PCORNET_CDM].PROCEDURES px
              where px.PATID = scr48.PATID and
                    -- CPT codes
                    (   px.px like '00868%'
                     or px.px in ('50300','50303','50305','50307','50308','50309','50340','50370','50380','50360','50365')) and
                    px.ADMIT_DATE < scr48.time_bd
              )
)
-- Burn Patients
    ,AKI_EXCLD_BURN_EN as (
select ENCOUNTERID
from #AKI_Initial aki
where exists (select 1 from [&&dbname].[&&PCORNET_CDM].DIAGNOSIS dx
              where dx.ENCOUNTERID = aki.ENCOUNTERID and
                    -- ICD9 for burn patients
                    ((dx.DX_TYPE = '09' and
                      (   dx.DX like '906.5%' 
                       or dx.DX like '906.6%'
                       or dx.DX like '906.7%'
                       or dx.DX like '906.8%'
                       or dx.DX like '906.9%'
                       or dx.DX like '940%'
                       or dx.DX like '941%'
                       or dx.DX like '942%'
                       or dx.DX like '943%'
                       or dx.DX like '944%'
                       or dx.DX like '945%'
                       or dx.DX like '946%'
                       or dx.DX like '947%'
                       or dx.DX like '948%'
                       or dx.DX like '949%')
                      ) or
                    -- ICD10 for burn patients
                     (dx.DX_TYPE = '10' and
                      (   dx.DX like 'T20.%' 
                       or dx.DX like 'T21.%'
                       or dx.DX like 'T22.%'
                       or dx.DX like 'T23.%'
                       or dx.DX like 'T24.%'
                       or dx.DX like 'T25.%'
                       or dx.DX like 'T26.%'
                       or dx.DX like 'T27.%'
                       or dx.DX like 'T28.%'
                       or dx.DX like 'T30.%'
                       or dx.DX like 'T31.%'
                       or dx.DX like 'T32.%')
                       )
                      ) and 
                      dx.DX_SOURCE = 'AD'
                )
)
-- collect all excluded encounters
SELECT * into #exclude_all FROM (
select ENCOUNTERID, 'Less_than_2_SCr' EXCLUD_TYPE from AKI_EXCLD_1SCR_EN
union all
select ENCOUNTERID, 'Initial_GFR_below_15' EXCLUD_TYPE from AKI_EXCLD_L1GFR_EN
union all 
select ENCOUNTERID, 'Pre_renal_failure' EXCLUD_TYPE from AKI_EXCLD_PRF_EN
union all
select ENCOUNTERID, 'Renal_transplant_within_48hr' EXCLUD_TYPE from AKI_EXCLD_RT48_EN
union all
select ENCOUNTERID, 'Burn_patients' EXCLUD_TYPE from AKI_EXCLD_BURN_EN) as tmp
