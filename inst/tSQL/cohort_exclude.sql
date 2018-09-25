/*******************************************************************************/
/*@file exclude.sql
/*
/*in: AKI_Scr_eGFR, AKI_Initial
/*
/*params: @dblink, &&dbname, &&PCORNET_CDM
/*
/*out: exclude_all
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
from $AKI_Scr_eGFR
where rn = 1 and eGFR <= 15
)
-- Pre-existing renal failure (DX)
    ,AKI_EXCLD_PRF_EN as (
select aki.ENCOUNTERID
from #AKI_Initial aki
where exists (select 1 from [@dblink].[&&dbname].[&&PCORNET_CDM].DIAGNOSIS dx
              where dx.PATID = aki.PATID and
                    -- ICD9 for renal failure
                    ((dx.DX_TYPE = '09' and
                      (   regexp_like(dx.DX,'586')
                       or regexp_like(dx.DX,'585\.9'))
                      ) or
                    -- ICD10 for renal failure
                     (dx.DX_TYPE = '10' and
                      (   regexp_like(dx.DX,'N18')
                       or regexp_like(dx.DX,'N19'))
                       )
                      ) and
                    dx.ADMIT_DATE < trunc(aki.ADMIT_DATE_TIME)
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
where exists (select 1 from [@dblink].[&&dbname].[&&PCORNET_CDM].DIAGNOSIS dx
              where dx.PATID = scr48.PATID and
                    -- ICD9 for RRT
                    ((dx.DX_TYPE = '09' and
                      (   regexp_like(dx.DX,'^996\.81')
                       or regexp_like(dx.DX,'^V42\.0'))
                      ) or
                    -- ICD10 for RRT
                     (dx.DX_TYPE = '10' and
                      (   regexp_like(dx.DX,'^Z94\.0')
                       or regexp_like(dx.DX,'^T86\.1[0|1|2]'))
                       )
                      ) and
                    dx.ADMIT_DATE < scr48.time_bd
                )
union
select distinct scr48.ENCOUNTERID
from scr48
where exists (select 1 from [@dblink].[&&dbname].[&&PCORNET_CDM].PROCEDURES px
              where px.PATID = scr48.PATID and
                    -- CPT codes
                    (   regexp_like(px.px,'00868')
                     or regexp_like(px.px,'5030[0|3|5|7|8|9]')
                     or regexp_like(px.px,'503[4|7|8]0')
                     or regexp_like(px.px,'5036[0|5]')) and
                    px.ADMIT_DATE < scr48.time_bd
              )
)
-- Burn Patients (UHC diagnosis, admit DRG)
    ,AKI_EXCLD_BURN_EN as (
select ENCOUNTERID
from #AKI_Initial
where exists (select 1 from [@dblink].[&&dbname].[&&PCORNET_CDM].DIAGNOSIS dx
              where dx.ENCOUNTERID = aki.ENCOUNTERID and
                    -- ICD9 for burn patients
                    ((dx.DX_TYPE = '09' and
                      (   regexp_like(dx.DX,'906\.[5-9]')
                       or regexp_like(dx.DX,'^94[0-9]'))
                      ) or
                    -- ICD10 for burn patients
                     (dx.DX_TYPE = '10' and
                      (   regexp_like(dx.DX,'^T2[0-8]')
                       or regexp_like(dx.DX,'^T3[0-2]'))
                       )
                      ) and
                    dx.DX_SOURCE = 'AD'
                )
)
-- collect all excluded encounters
select ENCOUNTERID, 'Less_than_2_SCr' EXCLUD_TYPE from AKI_EXCLD_1SCR_EN
union all
select ENCOUNTERID, 'Initial_GFR_below_15' EXCLUD_TYPE from AKI_EXCLD_L1GFR_EN
union all 
select ENCOUNTERID, 'Pre_renal_failure' EXCLUD_TYPE from AKI_EXCLD_PRF_EN
union all
select ENCOUNTERID, 'Renal_transplant_within_48hr' EXCLUD_TYPE from AKI_EXCLD_RT48_EN
union all
select ENCOUNTERID, 'Burn_patients' EXCLUD_TYPE from AKI_EXCLD_BURN_EN
into #exclude_all
