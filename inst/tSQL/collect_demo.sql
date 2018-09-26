/********************************************************************************/
/*@file collect_demo.sql
/*
/*in: AKI_onsets
/*
/*params: &&dbname, &&PCORNET_CDM
/*
/*out: AKI_DEMO
/*
/*action: query
/********************************************************************************/
select distinct
       pat.PATID
      ,pat.ENCOUNTERID
      ,demo.BIRTH_DATE
      ,datediff(yy,demo.BIRTH_DATE,pat.ADMIT_DATE) as AGE
      ,demo.SEX
      ,demo.RACE
      ,demo.HISPANIC
      ,dth.DEATH_DATE
      ,datediff(dd,pat.DISCHARGE_DATE,dth.DEATH_DATE) DDAYS_SINCE_ENC
      ,dth.DEATH_DATE_IMPUTE
      ,dth.DEATH_SOURCE
from AKI_onsets pat
left join [&&dbname].[&&PCORNET_CDM].DEMOGRAPHIC demo
on pat.PATID = demo.PATID
left join [&&dbname].[&&PCORNET_CDM].DEATH dth
on pat.PATID = dth.PATID
order by pat.PATID, pat.ENCOUNTERID


