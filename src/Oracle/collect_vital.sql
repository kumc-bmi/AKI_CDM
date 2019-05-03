/********************************************************************************/
/*@file collect_vital.sql
/*
/*in: AKI_onsets
/*
/*params: &&cdm_db_schema
/*
/*out: AKI_VITAL
/*
/*action: query
/********************************************************************************/
select pat.PATID
      ,pat.ENCOUNTERID
      --,v.VITALID
      ,to_date(to_char(trunc(v.MEASURE_DATE),'YYYY:MM:DD') || ' ' || to_char(v.MEASURE_TIME),
               'YYYY:MM:DD HH24:MI') MEASURE_DATE_TIME
      ,v.HT
      ,v.WT
      ,v.SYSTOLIC
      ,v.DIASTOLIC
      ,v.ORIGINAL_BMI
      ,v.SMOKING
      ,v.TOBACCO
      ,v.TOBACCO_TYPE
      ,round(v.MEASURE_DATE-pat.ADMIT_DATE) DAYS_SINCE_ADMIT
from AKI_onsets pat
left join &&cdm_db_schema.VITAL v
on pat.PATID = v.PATID
where v.MEASURE_DATE between pat.ADMIT_DATE-7 and coalesce(pat.AKI3_ONSET,pat.AKI2_ONSET,pat.AKI1_ONSET,pat.NONAKI_ANCHOR,pat.DISCHARGE_DATE) and
      coalesce(v.HT, v.WT, v.SYSTOLIC, v.DIASTOLIC, v.ORIGINAL_BMI) is not null
order by PATID, ENCOUNTERID, MEASURE_DATE_TIME


