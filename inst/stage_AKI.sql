/*******************************************************************************
@file stage_AKI.sql

require: 
 - AKI_eligible

out: 
 - AKI_stages_daily

action: 
 - write table


********************************************************************************/
with stage_aki as (
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
order by PATID, ENCOUNTERID, SPECIMEN_DATE_TIME
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
      ,row_number() over (partition by ENCOUNTERID, AKI_STAGE order by DAY_SINCE_ADMIT) rn
from stage_uni
order by PATID, ENCOUNTERID, AKI_STAGE, SPECIMEN_DATE
;