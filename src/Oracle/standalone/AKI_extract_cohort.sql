/*******************************************************************************
 AKI_extract_cohort.sql is used to extract the AKI cohort that satisfies the
 inclusion and exclusion criteria specified at: 
 https://github.com/kumc-bmi/AKI_CDM/blob/master/report/AKI_CDM_EXT_VALID_p1_QA.Rmd
 
 - &&cdm_db_schema will be substituted by corresponding CDM schema
********************************************************************************/

/******************************************************************************
 Collect initial cohort:
 - EI, IP, or IS
 - LOS >= 2
 - between &&start_date and Date &&end_date
    - first set &&start_date = '2010-01-01' and &&end_date = '2019-12-31'
    - if taking too much memory, reduce the time window
******************************************************************************/
create table AKI_Initial as
with age_at_admit as (
select e.ENCOUNTERID
      ,e.PATID
      ,to_date(to_char(trunc(e.ADMIT_DATE),'YYYY:MM:DD') || ' ' || to_char(e.ADMIT_TIME),
               'YYYY:MM:DD HH24:MI') ADMIT_DATE_TIME
      ,round((e.ADMIT_DATE-d.BIRTH_DATE)/365.25) age_at_admit
      ,to_date(to_char(trunc(e.DISCHARGE_DATE),'YYYY:MM:DD') || ' ' || to_char(e.DISCHARGE_TIME),
               'YYYY:MM:DD HH24:MI') DISCHARGE_DATE_TIME
      ,round(e.DISCHARGE_DATE - e.ADMIT_DATE) LOS
      ,row_number() over (partition by e.PATID,e.DISCHARGE_DATE order by e.ADMIT_DATE, e.ADMIT_TIME) rn /*merge admit dates*/
from &&cdm_db_schema.ENCOUNTER e
join &&cdm_db_schema.DEMOGRAPHIC d
on e.PATID = d.PATID
where e.DISCHARGE_DATE - e.ADMIT_DATE >= 2 and
      e.ENC_TYPE in ('EI','IP','IS') and
      e.ADMIT_DATE between Date '&&start_date' and Date '&&end_date'
)
select ENCOUNTERID
      ,PATID
      ,age_at_admit
      ,ADMIT_DATE_TIME
      ,DISCHARGE_DATE_TIME
      ,los
from age_at_admit
where age_at_admit >= 18 and rn = 1
;

/******************************************************************************
 Collect SCr and calculate eGFR for all eligible encounters
******************************************************************************/
create table All_Scr_eGFR as
with Scr_all as (
select l.PATID
      ,avg(l.RESULT_NUM) RESULT_NUM 
      ,l.LAB_ORDER_DATE
      ,l.SPECIMEN_DATE
      ,l.SPECIMEN_TIME
      ,l.RESULT_DATE
      ,l.RESULT_TIME
from &&cdm_db_schema.LAB_RESULT_CM l
where l.LAB_LOINC in ('2160-0','38483-4','14682-9','21232-4','35203-9','44784-7','59826-8') and 
      (UPPER(l.RESULT_UNIT) = 'MG/DL' or UPPER(l.RESULT_UNIT) = 'MG') and /*there are variations of common units*/
      l.SPECIMEN_SOURCE <> 'URINE' and  /*only serum creatinine*/
      l.RESULT_NUM > 0 and /*value 0 could exist*/
      exists (select 1 from AKI_Initial init where init.PATID = l.PATID)
group by l.PATID,l.ENCOUNTERID,l.LAB_ORDER_DATE,
         l.SPECIMEN_DATE,l.SPECIMEN_TIME,l.RESULT_DATE,l.RESULT_TIME
)
    ,Scr_w_age as (
select sa.PATID
      ,round((sa.LAB_ORDER_DATE - d.BIRTH_DATE)/365.25) AS age_at_Scr
      ,case when d.SEX = 'F' then 1 else 0 end as female_ind 
      ,case when d.RACE = '03' then 1 else 0 end as race_aa_ind /*03=Black or African American*/
      ,sa.RESULT_NUM
      ,sa.LAB_ORDER_DATE
      ,sa.SPECIMEN_DATE
      ,sa.SPECIMEN_TIME
      ,sa.RESULT_DATE
      ,sa.RESULT_TIME
from Scr_all sa
join &&cdm_db_schema.DEMOGRAPHIC d
on sa.PATID = d.PATID
)
select distinct 
       PATID
      ,RESULT_NUM SERUM_CREAT
      ,cast(175*power(RESULT_NUM,-1.154)*power(age_at_Scr,-0.203)*(0.742*female_ind+(1-female_ind))*(1.212*race_aa_ind+(1-race_aa_ind)) as BINARY_FLOAT) eGFR 
      ,LAB_ORDER_DATE
      ,to_date(to_char(SPECIMEN_DATE,'YYYY:MM:DD') || ' ' || to_char(SPECIMEN_TIME),
               'YYYY:MM:DD HH24:MI') SPECIMEN_DATE_TIME
      ,to_date(to_char(RESULT_DATE,'YYYY:MM:DD') || ' ' || to_char(RESULT_TIME),
               'YYYY:MM:DD HH24:MI') RESULT_DATE_TIME
from Scr_w_age
where age_at_Scr >= 18
;

/******************************************************************************
 Merge labs within the same encounter and filter out encounters with less than
 2 SCr records
******************************************************************************/
create table AKI_Scr_eGFR as
with multi_match as (
select aki.PATID
      ,aki.ENCOUNTERID /*merge lab encounters*/
      ,scr.SERUM_CREAT
      ,scr.eGFR
      ,scr.LAB_ORDER_DATE
      ,scr.SPECIMEN_DATE_TIME
      ,scr.RESULT_DATE_TIME
      ,aki.ADMIT_DATE_TIME
from All_Scr_eGFR scr
join AKI_Initial aki
on scr.PATID = aki.PATID and
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
      ,ADMIT_DATE_TIME
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
      ,ADMIT_DATE_TIME
      ,rn
from scr_cnt
where scr_tot > 1
;

/******************************************************************************
 Calculate Baseline SCr: first SCr at encounter
******************************************************************************/
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
group by scrb.PATID,scrb.ENCOUNTERID,init.ADMIT_DATE_TIME,init.DISCHARGE_DATE_TIME,scrb.LAB_ORDER_DATE,scrb.SPECIMEN_DATE_TIME,scrb.RESULT_DATE_TIME
;

/***************************************************
 Exclusion Criteria 
 ****************************************************/
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
                       or dx.DX like '996.81%')
                      ) or
                    -- ICD10 for RRT or dialysis
                     (dx.DX_TYPE = '10' and
                      (   dx.DX like 'Z49.31%'
                       or dx.DX like 'Z99.2%'
                       or dx.DX like 'Z94.0%'
                       or dx.DX in ('T86.10', 'T86.11', 'T86.12'))
                       )
                      ) and
                    dx.ADMIT_DATE < trunc(aki.ADMIT_DATE_TIME)
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
                     px.PX_DATE < scr48.time_bd and px.PX_DATE >= scr48.admit_date
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
                      dx.ADMIT_DATE = trunc(aki.ADMIT_DATE_TIME) and
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
;

/*************************************************
 Finalize the Eligbile Encounters 
 *************************************************/
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
      ,scrb.DISCHARGE_DATE_TIME
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
;

/******************************************************************************
 AKI Staging
******************************************************************************/
create table AKI_stages_daily as
with aki3_rrt as (
-- identify 3-stage AKI based on existence of RRT
select akie.PATID
      ,akie.ENCOUNTERID
      ,akie.ADMIT_DATE_TIME
      ,akie.SERUM_CREAT_BASE
      ,akie.SPECIMEN_DATE_TIME_BASE
      ,min(px.PX_DATE) SPECIMEN_DATE_TIME
from AKI_eligible akie
join &&cdm_db_schema.PROCEDURES px
on px.PATID = akie.PATID and
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
  px.PX_DATE between akie.ADMIT_DATE_TIME and akie.DISCHARGE_DATE_TIME
group by akie.PATID,akie.ENCOUNTERID,akie.ADMIT_DATE_TIME,akie.SERUM_CREAT_BASE,akie.SPECIMEN_DATE_TIME_BASE
)
  ,stage_aki as (
-- a semi-cartesian self-join to identify all eligible 1-, 3-stages w.r.t rolling baseline
select distinct
       s1.PATID
      ,s1.ENCOUNTERID
      ,s1.ADMIT_DATE_TIME
      ,s1.SERUM_CREAT_BASE
      ,s1.SPECIMEN_DATE_TIME_BASE SERUM_CREAT_BASE_DATE_TIME
      ,s1.SERUM_CREAT SERUM_CREAT_RBASE
      ,s2.SERUM_CREAT
      ,s2.SERUM_CREAT - s1.SERUM_CREAT SERUM_CREAT_INC
      ,case when s2.SERUM_CREAT - s1.SERUM_CREAT >= 0.3 then 1
            when s2.SERUM_CREAT > 4.0 then 3
            else 0
       end as AKI_STAGE
      ,s2.SPECIMEN_DATE_TIME
      ,s2.RESULT_DATE_TIME
from AKI_eligible s1
join AKI_eligible s2
on s1.ENCOUNTERID = s2.ENCOUNTERID
--restrict s2 to be strictly after s1 and before s1+2d
where s2.SPECIMEN_DATE_TIME - s1.SPECIMEN_DATE_TIME <= 2 and
      s2.SPECIMEN_DATE_TIME - s1.SPECIMEN_DATE_TIME > 0
union all
-- identify 1-,2-,3-stage AKI compared to baseline
select distinct 
       PATID
      ,ENCOUNTERID
      ,ADMIT_DATE_TIME
      ,SERUM_CREAT_BASE
      ,SPECIMEN_DATE_TIME_BASE SERUM_CREAT_BASE_DATE_TIME
      ,null SERUM_CREAT_RBASE
      ,SERUM_CREAT
      ,round(SERUM_CREAT/SERUM_CREAT_BASE,1) SERUM_CREAT_INC
      ,case when round(SERUM_CREAT/SERUM_CREAT_BASE,1) between 1.5 and 1.9 then 1
            when round(SERUM_CREAT/SERUM_CREAT_BASE,1) between 2.0 and 2.9 then 2
            when round(SERUM_CREAT/SERUM_CREAT_BASE,1) >= 3 then 3
            else 0
       end as AKI_STAGE
      ,SPECIMEN_DATE_TIME
      ,RESULT_DATE_TIME
from AKI_eligible 
where SPECIMEN_DATE_TIME_BASE - ADMIT_DATE_TIME >= 0 and
      SPECIMEN_DATE_TIME - SPECIMEN_DATE_TIME_BASE <= 7 and
      SPECIMEN_DATE_TIME - SPECIMEN_DATE_TIME_BASE > 0
union all
select rrt.PATID
      ,rrt.ENCOUNTERID
      ,rrt.ADMIT_DATE_TIME
      ,rrt.SERUM_CREAT_BASE
      ,rrt.SPECIMEN_DATE_TIME_BASE SERUM_CREAT_BASE_DATE_TIME
      ,null SERUM_CREAT_RBASE
      ,null SERUM_CREAT
      ,null SERUM_CREAT_INC
      ,3 as AKI_STAGE
      ,rrt.SPECIMEN_DATE_TIME
      ,null RESULT_DATE_TIME
from aki3_rrt rrt
)
   ,AKI_stages as (
select PATID
      ,ENCOUNTERID
      ,ADMIT_DATE_TIME
      ,SERUM_CREAT_BASE
      ,SERUM_CREAT_BASE_DATE_TIME
      ,SERUM_CREAT_RBASE
      ,SERUM_CREAT
      ,SERUM_CREAT_INC
      ,AKI_STAGE
      ,SPECIMEN_DATE_TIME
      ,round((SPECIMEN_DATE_TIME - ADMIT_DATE_TIME)*24) HOUR_SINCE_ADMIT
      ,floor((SPECIMEN_DATE_TIME - ADMIT_DATE_TIME)*2) HDAY_SINCE_ADMIT
      ,floor((SPECIMEN_DATE_TIME - ADMIT_DATE_TIME)) DAY_SINCE_ADMIT
      ,dense_rank() over (partition by PATID, ENCOUNTERID, floor((SPECIMEN_DATE_TIME - ADMIT_DATE_TIME)*2)
                          order by floor((SPECIMEN_DATE_TIME - ADMIT_DATE_TIME)*2) asc, 
                                   AKI_STAGE desc, SERUM_CREAT desc, SERUM_CREAT_INC desc) rn_hday
      ,dense_rank() over (partition by PATID, ENCOUNTERID, floor((SPECIMEN_DATE_TIME - ADMIT_DATE_TIME))
                          order by floor((SPECIMEN_DATE_TIME - ADMIT_DATE_TIME)) asc,
                                   AKI_STAGE desc, SERUM_CREAT desc, SERUM_CREAT_INC desc) rn_day
from stage_aki
)
  ,stage_uni as (
select distinct 
       PATID
      ,ENCOUNTERID
      ,ADMIT_DATE_TIME
      ,SERUM_CREAT_BASE
      ,SERUM_CREAT_BASE_DATE_TIME
      ,SERUM_CREAT_RBASE
      ,SERUM_CREAT
      ,SERUM_CREAT_INC
      ,AKI_STAGE
      ,SPECIMEN_DATE_TIME
      ,DAY_SINCE_ADMIT
from AKI_stages
where rn_day = 1
)
select distinct 
       PATID
      ,ENCOUNTERID
      ,ADMIT_DATE_TIME
      ,SERUM_CREAT_BASE
      ,SERUM_CREAT_BASE_DATE_TIME
      ,SERUM_CREAT_RBASE
      ,SERUM_CREAT
      ,SERUM_CREAT_INC
      ,AKI_STAGE
      ,trunc(SPECIMEN_DATE_TIME) SPECIMEN_DATE
      ,DAY_SINCE_ADMIT
      ,row_number() over (partition by ENCOUNTERID, AKI_STAGE order by DAY_SINCE_ADMIT) rn_asc
      ,row_number() over (partition by ENCOUNTERID, AKI_STAGE order by DAY_SINCE_ADMIT desc) rn_desc
      ,max(AKI_STAGE) over (partition by ENCOUNTERID) AKI_STAGE_max
from stage_uni
order by PATID, ENCOUNTERID, AKI_STAGE, SPECIMEN_DATE
;

create table AKI_onsets as
with pat_enc as (
select distinct 
       PATID 
      ,ENCOUNTERID 
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
 where rn_asc = 1 and AKI_STAGE_max > 0 and AKI_STAGE > 0
 union all
 select ENCOUNTERID
       ,AKI_STAGE
       ,SPECIMEN_DATE
 from AKI_stages_daily
 where rn_desc = 1 and AKI_STAGE_max = 0 and AKI_STAGE = 0
 )
pivot 
(min(SPECIMEN_DATE)
 for AKI_STAGE in (0 as NONAKI_ANCHOR,
                   1 as AKI1_ONSET,
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
 where rn_asc = 1 and AKI_STAGE_max > 0 and AKI_STAGE > 0
 union all
 select ENCOUNTERID
       ,AKI_STAGE
       ,SERUM_CREAT
 from AKI_stages_daily
 where rn_desc = 1 and AKI_STAGE_max = 0 and AKI_STAGE = 0)
pivot 
(max(SERUM_CREAT)
 for AKI_STAGE in (0 as NON_AKI_SCR,
                   1 as AKI1_SCR,
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
 where rn_asc = 1 and AKI_STAGE_max > 0 and AKI_STAGE > 0
 union all
 select ENCOUNTERID
       ,AKI_STAGE
       ,SERUM_CREAT_INC
 from AKI_stages_daily
 where rn_desc = 1 and AKI_STAGE_max = 0 and AKI_STAGE = 0)
pivot 
(max(SERUM_CREAT_INC)
 for AKI_STAGE in (0 as NON_AKI_INC,
                   1 as AKI1_INC,
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
join AKI_Initial init
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
      ,to_char(ENCOUNTERID) ENCOUNTERID
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
from raw_onset
order by PATID, ENCOUNTERID
;

/*************************
 Consort Diagram
***************************/
create table consort_diagram as
select 'Initial' CNT_TYPE,
       count(distinct encounterid) ENC_CNT
from AKI_Initial
union all
select 'Has_at_least_2_SCr' CNT_TYPE,
       count(distinct encounterid) ENC_CNT
from AKI_Scr_eGFR
union all
select exclud_type CNT_TYPE,
       count(distinct encounterid) ENC_CNT
from exclude_all
group by exclud_type
union all
select 'Total' CNT_TYPE,
       count(distinct encounterid) ENC_CNT
from AKI_onsets
union all
select 'nonAKI' CNT_TYPE,
       count(distinct encounterid) ENC_CNT
from AKI_onsets
where AKI1_onset is null and
      AKI2_onset is null and
      AKI3_onset is null
union all
select 'AKI1' CNT_TYPE,
       count(distinct encounterid) ENC_CNT
from AKI_onsets
where AKI1_onset is not null
union all
select 'nonAKI_to_AKI2' CNT_TYPE,
       count(distinct encounterid) ENC_CNT
from AKI_onsets
where AKI1_onset is null and
      AKI2_onset is not null
union all
select 'AKI1_to_AKI2' CNT_TYPE,
       count(distinct encounterid) ENC_CNT
from AKI_onsets
where AKI1_onset is not null and
      AKI2_onset is not null
union all
select 'nonAKI_to_AKI3' CNT_TYPE,
       count(distinct encounterid) ENC_CNT
from AKI_onsets
where AKI1_onset is null and
      AKI2_onset is null and
      AKI3_onset is not null
union all
select 'nonAKI_to_AKI2_to_AKI3' CNT_TYPE,
       count(distinct encounterid) ENC_CNT
from AKI_onsets
where AKI1_onset is null and
      AKI2_onset is not null and
      AKI3_onset is not null
union all
select 'AKI1_to_AKI2_to_AKI3' CNT_TYPE,
       count(distinct encounterid) ENC_CNT
from AKI_onsets
where AKI1_onset is not null and
      AKI2_onset is not null and
      AKI3_onset is not null
;

--------------------------------------------------------------------------------------------
/*export the following tables:
 - AKI_onsets (Note: don't drop this table until the AKI_collect_data.sql is successfully run)
 - consort_diagram
    - send consort_diagram to KUMC for an initial diagnostic
    - upon approval, drop the following intermediate tables and run AKI_collect_data.sql
*/
--------------------------------------------------------------------------------------------

/*drop the following intermediate tables when `AKI_onsets` table looks ok*/
--drop table AKI_Initial purge;
--drop table All_Scr_eGFR purge;
--drop table AKI_Scr_eGFR purge;
--drop table AKI_Scr_base purge;
--drop table exclude_all purge;
--drop table AKI_eligible purge;
--drop table AKI_stage_daily purge;
--drop table consort_diagram purge;


