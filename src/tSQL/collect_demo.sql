/********************************************************************************/
/*@file collect_demo.sql
/*
/*in: AKI_onsets
/*
/*params: &&cdm_db_name, &&cdm_db_schema
/*
/*out: AKI_DEMO
/*
/*action: query
/********************************************************************************/
select distinct
       pat.PATID
      ,pat.ENCOUNTERID
      ,demo.BIRTH_DATE
      ,(CONVERT(int,CONVERT(char(8),pat.ADMIT_DATE,112))-CONVERT(int,CONVERT(char(8),demo.BIRTH_DATE,112)))/10000 AS AGE
      ,demo.SEX
      ,demo.RACE
      ,demo.HISPANIC
      ,dth.DEATH_DATE
      ,datediff(dd,pat.DISCHARGE_DATE,dth.DEATH_DATE) DDAYS_SINCE_ENC
      ,dth.DEATH_DATE_IMPUTE
      ,dth.DEATH_SOURCE
from #AKI_onsets pat
left join [&&cdm_db_name].[&&cdm_db_schema].DEMOGRAPHIC demo
on pat.PATID = demo.PATID
left join [&&cdm_db_name].[&&cdm_db_schema].DEATH dth
on pat.PATID = dth.PATID
order by pat.PATID, pat.ENCOUNTERID
