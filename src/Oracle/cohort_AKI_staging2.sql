/*******************************************************************************/
/*@file cohort_AKI_staging.sql
/*
/*in: AKI_eligible
/*
/*out: AKI_stages_daily
/*
/*action: write
/********************************************************************************/
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
on px.ENCOUNTERID = akie.ENCOUNTERID and
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
  )
group by akie.PATID,akie.ENCOUNTERID,akie.ADMIT_DATE_TIME,akie.SERUM_CREAT_BASE,akie.SPECIMEN_DATE_TIME_BASE
)
     ,stage_aki as (
-- a semi-cartesian join to identify all eligible 1-, 3-stages w.r.t rolling baseline
select distinct
       s1.PATID
      ,s1.ENCOUNTERID
      ,s1.ADMIT_DATE_TIME
      ,s1.SERUM_CREAT_BASE
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
-- only compare to baseline (baseline before admission)
select distinct 
       PATID
      ,ENCOUNTERID
      ,ADMIT_DATE_TIME
      ,SERUM_CREAT_BASE
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
where SPECIMEN_DATE_TIME_BASE - ADMIT_DATE_TIME < 0 and
      SPECIMEN_DATE_TIME - ADMIT_DATE_TIME <= 7 and
      SPECIMEN_DATE_TIME - ADMIT_DATE_TIME > 0
union all
-- only compare to baseline (baseline after or on admission)
select distinct 
       PATID
      ,ENCOUNTERID
      ,ADMIT_DATE_TIME
      ,SERUM_CREAT_BASE
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
)
   ,AKI_stages as (
select PATID
      ,ENCOUNTERID
      ,ADMIT_DATE_TIME
      ,SERUM_CREAT_BASE
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

