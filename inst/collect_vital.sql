/********************************************************************************/
/*@file collect_vital.sql
/*
/*in: AKI_onsets
/*
/*params: &&PCORNET_CDM, @server
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
      ,case when v.SMOKING = 'NI' then null else v.SMOKING end as SMOKING
      ,case when v.TOBACCO = 'NI' then null else v.TOBACCO end as TOBACCO
      ,case when v.TOBACCO_TYPE = 'NI' then null else v.TOBACCO_TYPE end as TOBACCO_TYPE
from AKI_onsets pat
left join &&PCORNET_CDM.VITA@serverL v
on pat.PATID = v.PATID
where v.MEASURE_DATE between pat.ADMIT_DATE-7 and pat.DISCHARGE_DATE and
      coalesce(v.HT, v.WT, v.SYSTOLIC, v.DIASTOLIC, v.ORIGINAL_BMI) is not null
order by PATID, ENCOUNTERID, MEASURE_DATE_TIME


