/********************************************************************************/
/*@file get_final_cohort.sql
/*
/*in: AKI_onsets
/*
/*params: &&PCORNET_CDM, @server
/*
/*out: AKI_DEMO
/*
/*action: query
/********************************************************************************/
select distinct
       pat.PATID
      ,pat.ENCOUNTERID
      ,demo.BIRTH_DATE
      ,datediff(demo.BIRTH_DATE,pat.ADMIT_DATE) AGE
      ,demo.SEX
      ,demo.RACE
      ,demo.HISPANIC
      ,dth.DEATH_DATE
      ,datediff(dd,pat.DISCHARGE_DATE,dth.DEATH_DATE) DDAYS_SINCE_ENC
      ,dth.DEATH_DATE_IMPUTE
      ,dth.DEATH_SOURCE
from AKI_onsets pat
left join [@server].[&&PCORNET_CDM].DEMOGRAPHIC demo
on pat.PATID = demo.PATID
left join [@server].[&&PCORNET_CDM].DEATH dth
on pat.PATID = dth.PATID
order by pat.PATID, pat.ENCOUNTERID


