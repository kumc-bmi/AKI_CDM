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
      ,round((pat.ADMIT_DATE - demo.BIRTH_DATE)/365.25) AGE
      ,demo.SEX
      ,demo.RACE
      ,demo.HISPANIC
from AKI_onsets pat
left join &&PCORNET_CDM.DEMOGRAPHIC demo
on pat.PATID = demo.PATID
order by pat.PATID
;

