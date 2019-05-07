/*******************************************************************************/
/*@file cohort_exclude.sql
/*
/*in: AKI_Scr_eGFR, AKI_Initial
/*
/*params: &&cdm_db_schema
/*
/*out: exclude_all
/*
/*action: write
/********************************************************************************/
create table exclude_all as
-- At CKD stage 4 or higher
with AKI_EXCLD_L1GFR_EN as (
select distinct ENCOUNTERID
from AKI_Scr_eGFR
where rn = 1 and eGFR < 15
)
-- update AKI_initial 
    ,AKI_init as (
select * from AKI_Initial init
where exists (select 1 from AKI_Scr_eGFR scr2
              where scr2.ENCOUNTERID = init.ENCOUNTERID)
)
-- Pre-existing ESRD
    ,AKI_EXCLD_PRF_EN as (
select aki.ENCOUNTERID
from AKI_init aki
where exists (select 1 from &&cdm_db_schema.DIAGNOSIS dx
              where dx.PATID = aki.PATID and
                    -- ICD9 for ESRD
                    ((dx.DX_TYPE = '09' and
                      (   regexp_like(dx.DX,'585\.6'))
                      ) or
                    -- ICD10 for ESRD
                     (dx.DX_TYPE = '10' and
                      (   regexp_like(dx.DX,'N18\.6'))
                       )
                      ) and
                    dx.ADMIT_DATE < trunc(aki.ADMIT_DATE_TIME)
                )
)
-- Pre-existing dialysis or renal transplantation
    ,AKI_EXCLD_PRRT_EN as (
select aki.ENCOUNTERID
from AKI_init aki
where exists (select 1 from &&cdm_db_schema.DIAGNOSIS dx
              where dx.PATID = aki.PATID and
                    -- ICD9 for ESRD
                    ((dx.DX_TYPE = '09' and
                      (   regexp_like(dx.DX,'V45\.11')
                       or regexp_like(dx.DX,'V56\.0'))
                      ) or
                    -- ICD10 for ESRD
                     (dx.DX_TYPE = '10' and
                      (   regexp_like(dx.DX,'Z49\.31')
                       or regexp_like(dx.DX,'Z99\.2'))
                       )
                      ) and
                    dx.ADMIT_DATE < trunc(aki.ADMIT_DATE_TIME)
                )
union all
select aki.ENCOUNTERID
from AKI_init aki
where exists (select 1 from &&cdm_db_schema.PROCEDURES px
              where px.PATID = aki.PATID and
                    -- CPT codes
                    (
                     (px.PX_TYPE = 'CH' and   
                      (   px.px in ('99512','90970','90989')
                       or regexp_like(px.px,'9092[0|1|4|5]')
                       or regexp_like(px.px,'9093[5|7]')
                       or regexp_like(px.px,'9094[5|7]')
                       or regexp_like(px.px,'9096[0|1|2|6]')
                       or regexp_like(px.px,'9099[3|9]')
                       )
                      ) or
                    -- ICD9 codes
                     (px.PX_TYPE = '09' and
                      (  regexp_like(px.px,'39\.9[3|5]')
                      or regexp_like(px.px,'54\.98')
                       )
                      ) or
                     -- ICD10 codes
                     (px.PX_TYPE = '10' and
                      (  regexp_like(px.px,'031[3|4|5|6|7|8|]0JD')
                      or regexp_like(px.px,'031[A|B|C|9]0JF')
                       )
                      )
                     ) and
                     px.PX_DATE < trunc(aki.ADMIT_DATE_TIME)
 )
)
-- Receive renal transplant withing 48 hr since 1st Scr (PX, DX)
    ,scr48 as (
select PATID, ENCOUNTERID,
       SPECIMEN_DATE_TIME+2 time_bd
from AKI_Scr_eGFR
where rn = 1
)
    ,AKI_EXCLD_RT48_EN as (
select distinct scr48.ENCOUNTERID
from scr48
where exists (select 1 from &&cdm_db_schema.PROCEDURES px
              where px.ENCOUNTERID = scr48.ENCOUNTERID and
                    -- CPT codes
                    (
                     (px.PX_TYPE = 'CH' and   
                      (   px.px in ('99512','90970','90989')
                       or regexp_like(px.px,'9092[0|1|4|5]')
                       or regexp_like(px.px,'9093[5|7]')
                       or regexp_like(px.px,'9094[5|7]')
                       or regexp_like(px.px,'9096[0|1|2|6]')
                       or regexp_like(px.px,'9099[3|9]')
                       )
                      ) or
                    -- ICD9 codes
                     (px.PX_TYPE = '09' and
                      (  regexp_like(px.px,'39\.9[3|5]')
                      or regexp_like(px.px,'54\.98')
                       )
                      ) or
                     -- ICD10 codes
                     (px.PX_TYPE = '10' and
                      (  regexp_like(px.px,'031[3|4|5|6|7|8|]0JD')
                      or regexp_like(px.px,'031[A|B|C|9]0JF')
                       )
                      )
                     ) and
                     px.PX_DATE < scr48.time_bd
              )
)
-- Burn Patients
    ,AKI_EXCLD_BURN_EN as (
select distinct aki.ENCOUNTERID
from AKI_init aki
where exists (select 1 from &&cdm_db_schema.DIAGNOSIS dx
              where dx.ENCOUNTERID = aki.ENCOUNTERID and
                    -- ICD9 for burn patients
                    ((dx.DX_TYPE = '09' and
                      (   regexp_like(dx.DX,'906\.[5-9]')
                       or regexp_like(dx.DX,'^94[0-9]'))
                      ) or
                    -- ICD10 for burn patients
                     (dx.DX_TYPE = '10' and
                      (   regexp_like(dx.DX,'^T2[0-8]\.')
                       or regexp_like(dx.DX,'^T3[0-2]\.'))
                       )
                      ) and 
                      dx.DX_SOURCE = 'AD'
                )
)
-- collect all excluded encounters
select ENCOUNTERID, 'Initial_GFR_below_15' EXCLUD_TYPE from AKI_EXCLD_L1GFR_EN
union all 
select ENCOUNTERID, 'Pre_ESRD' EXCLUD_TYPE from AKI_EXCLD_PRF_EN
union all
select ENCOUNTERID, 'Pre_RRT' EXCLUD_TYPE from AKI_EXCLD_PRRT_EN
union all
select ENCOUNTERID, 'RRT_within_48hr' EXCLUD_TYPE from AKI_EXCLD_RT48_EN
union all
select ENCOUNTERID, 'Burn_patients' EXCLUD_TYPE from AKI_EXCLD_BURN_EN

