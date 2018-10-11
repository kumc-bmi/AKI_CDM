/********************************************************************************/
/*@file collect_vital.sql
/*
/*in: #AKI_onsets
/*
/*params: &&dbname, &&PCORNET_CDM
/*
/*out: AKI_VITAL
/*
/*action: query
/********************************************************************************/
select pat.PATID
      ,pat.ENCOUNTERID
      --,v.VITALID
      ,convert(datetime, 
               convert(CHAR(8), v.MEASURE_DATE, 112)+ ' ' + CONVERT(CHAR(8), v.MEASURE_TIME, 108)
               ) MEASURE_DATE_TIME
      ,v.HT
      ,v.WT
      ,v.SYSTOLIC
      ,v.DIASTOLIC
      ,v.ORIGINAL_BMI
      ,case when v.SMOKING = 'NI' then null else v.SMOKING end as SMOKING
      ,case when v.TOBACCO = 'NI' then null else v.TOBACCO end as TOBACCO
      ,case when v.TOBACCO_TYPE = 'NI' then null else v.TOBACCO_TYPE end as TOBACCO_TYPE
      ,datediff(dd,pat.ADMIT_DATE,v.MEASURE_DATE) DAYS_SINCE_ADMIT
from #AKI_onsets pat
left join [&&dbname].[&&PCORNET_CDM].VITAL v
on pat.PATID = v.PATID
where v.MEASURE_DATE between dateadd(day,-7,pat.ADMIT_DATE) and pat.DISCHARGE_DATE and
      coalesce(v.HT, v.WT, v.SYSTOLIC, v.DIASTOLIC, v.ORIGINAL_BMI) is not null
order by PATID, ENCOUNTERID, MEASURE_DATE_TIME
