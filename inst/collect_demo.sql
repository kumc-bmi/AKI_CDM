/********************************************************************************/
/*@file get_final_cohort.sql
/*
/*in: AKI_onsets
/*
/*params: &&PCORNET_CDM
/*
/*out: AKI_DEMO
/*
/*action: query
/********************************************************************************/
select distinct
       pat.PATID
      ,pat.ENCOUNTERID
      ,demo.BIRTH_DATE
      ,round((pat.ADMIT_DATE - demo.BIRTH_DATE)/365.25) AGE
      ,demo.SEX
      ,demo.RACE
      ,demo.HISPANIC
      ,dth.DEATH_DATE
      ,(dth.DEATH_DATE - pat.DISCHARGE_DATE) DDAYS_SINCE_ENC
      ,dth.DEATH_DATE_IMPUTE
      ,dth.DEATH_SOURCE
from AKI_onsets pat
left join &&PCORNET_CDM.DEMOGRAPHIC demo
on pat.PATID = demo.PATID
left join &&PCORNET_CDM.DEATH dth
on pat.PATID = dth.PATID
order by pat.PATID, pat.ENCOUNTERID


