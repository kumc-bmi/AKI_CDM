/********************************************************************************/
/*@file collect_vital.sql
/*
/*in: AKI_onsets
/*
/*params: &&PCORNET_CDM
/*
/*out: AKI_VITAL
/*
/*action: query
/********************************************************************************/
select pat.PATID
      ,pat.ENCOUNTERID
      ,v.VITALID
      ,to_date(to_char(trunc(v.MEASURE_DATE),'YYYY:MM:DD') || ' ' || to_char(v.MEASURE_TIME),
               'YYYY:MM:DD HH24:MI') MEASURE_DATE_TIME
      ,v.HT
      ,v.WT
      ,v.SYSTOLIC
      ,v.DIASTOLIC
      ,v.ORIGINAL_BMI
      ,case when v.SMOKING = 'NI' then null else v.SMOKING end as SMOKING
      ,case when v.TOBACCO = 'NI' then null else v.TOBACCO end as TOBACCO
      ,case when v.TOBACCO_TYPE = 'NI' then null else v.TOBACCO_TYPE end as TOBACCO_TYPE
from AKI_onsets pat
left join &&PCORNET_CDM.VITAL v
on pat.ENCOUNTERID = v.ENCOUNTERID
where v.MEASURE_DATE is not null and v.MEASURE_TIME is not null and
      coalesce(v.HT, v.WT, v.SYSTOLIC, v.DIASTOLIC, v.ORIGINAL_BMI) is not null and
      coalesce(v.SMOKING, v.TOBACCO, v.TOBACCO_TYPE) is not null
order by PATID, ENCOUNTERID, MEASURE_DATE_TIME


