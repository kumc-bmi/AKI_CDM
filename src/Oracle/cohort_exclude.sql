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
                      (   dx.DX like '%585.6%')
                      ) or
                    -- ICD10 for ESRD
                     (dx.DX_TYPE = '10' and
                      (   dx.DX like '%N18.6%')
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
                    -- ICD9 for RRT or dialysis
                    ((dx.DX_TYPE = '09' and
                      (   dx.DX like 'V45.1%'
                       or dx.DX like 'V56.%'
                       or dx.DX like 'V42.0%'
                       or dx.DX like '996.81%'
                      ) or
                    -- ICD10 for RRT or dialysis
                     (dx.DX_TYPE = '10' and
                      (   dx.DX like 'Z49.31%'
                       or dx.DX like 'Z99.2%'
                       or dx.DX like 'Z94.0%'
                       or dx.DX in ('T86.10', 'T86.11', 'T86.12')
                       )
                      ) and
                    dx.ADMIT_DATE < date_trunc('day',aki.ADMIT_DATE_TIME::timestamp)
                )
union all
select aki.ENCOUNTERID
from AKI_init aki
where exists (select 1 from &&cdm_db_schema.PROCEDURES px
              where px.PATID = aki.PATID and
                    -- CPT codes for RRT or dialysis
                    -- ref: https://www.cms.gov/Regulations-and-Guidance/Guidance/Transmittals/downloads/R1810B3.pdf
                    -- ref: https://www.outsourcestrategies.com/blog/coding-kidney-transplantation-overview.html 
                    (
                     (px.PX_TYPE = 'CH' and   
                      (   px.px in ('90935','90937') -- dialysis
                       or px.px like '9094%'
                       or px.px like '9095%'
                       or px.px like '9096%'
                       or px.px like '9097%'
                       or px.px like '9098%'
                       or px.px like '9099%'
                       or px.px in ('50300','50320','50323','50325','50327','50328','50329',
                                    '50340','50360','50365','50370','50380') --RRT
                       )
                      ) or
                    -- ICD9 codes for RRT or dialysis
                    -- ref:https://www.sentinelinitiative.org/assessments/drugs/dialysis-icd9-procedure-codes
                    -- ref:https://www.outsourcestrategies.com/blog/coding-kidney-transplantation-overview.html 
                     (px.PX_TYPE = '09' and
                      (   px.px in ('39.93','39.95','54.98') -- dialysis
                       or px.px in ('55.51','55.52','55.53','55.54','55.61','55.69') --RRT
                       )
                      ) or
                     -- ICD10 codes for RRT or dialysis
                     -- ref: https://icd.codes/icd10pcs/0TY 
                     -- ref: https://icd.codes/icd10pcs/5A1
                     -- ref:https://www.outsourcestrategies.com/blog/coding-kidney-transplantation-overview.html 
                     (px.PX_TYPE = '10' and
                      (  px.px in ('5A1D00Z','5A1D60Z','5A1D70Z','5A1D80Z','5A1D90Z') -- dialysis
                      or px.px in ('0TY00Z0','0TY00Z1','0TY00Z2','0TY10Z0','0TY10Z1','0TY10Z2',
                                   '0TB00ZZ','0TB10ZZ','0TT00ZZ','0TT10ZZ','0TT20ZZ') -- RRT
                       )
                      )
                     ) and
                     px.PX_DATE < trunc(aki.ADMIT_DATE_TIME)
 )
)
-- Receive renal transplant withing 48 hr since 1st Scr (PX, DX)
    ,scr48 as (
select PATID, ENCOUNTERID, trunc(ADMIT_DATE_TIME) admit_date,
       SPECIMEN_DATE_TIME+2 time_bd
from AKI_Scr_eGFR
where rn = 1
)
    ,AKI_EXCLD_RT48_EN as (
select distinct scr48.ENCOUNTERID
from scr48
where exists (select 1 from &&cdm_db_schema.PROCEDURES px
              where px.PATID = scr48.PATID and
                    (
                     (px.PX_TYPE = 'CH' and   
                      (   px.px in ('50300','50320','50323','50325','50327','50328','50329',
                                    '50340','50360','50365','50370','50380') --RRT
                       )
                      ) or
                    -- ICD9 codes for RRT
                     (px.PX_TYPE = '09' and
                      (   px.px in ('55.51','55.52','55.53','55.54','55.61','55.69') --RRT
                       )
                      ) or
                     -- ICD10 codes for RRT
                     (px.PX_TYPE = '10' and
                      (  px.px in ('0TY00Z0','0TY00Z1','0TY00Z2','0TY10Z0','0TY10Z1','0TY10Z2',
                                   '0TB00ZZ','0TB10ZZ','0TT00ZZ','0TT10ZZ','0TT20ZZ') -- RRT
                       )
                      )
                     ) and
                     px.PX_DATE < scr48.time_bd and px.PX_DATE >= admit_date
              )
)
-- Burn Patients
    ,AKI_EXCLD_BURN_EN as (
select distinct aki.ENCOUNTERID
from AKI_init aki
where exists (select 1 from &&cdm_db_schema.DIAGNOSIS dx
              where dx.PATID = aki.PATID and
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
                      dx.ADMIT_DATE = trunc(init.ADMIT_DATE_TIME) and
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

