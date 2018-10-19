/********************************************************************************/
/*@file collect_lab.sql
/*
/*in: #AKI_onsets
/*
/*params: &&dbname, &&PCORNET_CDM
/*
/*out: AKI_LAB
/*
/*action: query
/********************************************************************************/
select distinct
       pat.PATID
      ,pat.ENCOUNTERID
      ,l.LAB_ORDER_DATE
      ,convert(datetime, 
               convert(CHAR(8), l.SPECIMEN_DATE, 112)+ ' ' + CONVERT(CHAR(8), l.SPECIMEN_TIME, 108)
               ) SPECIMEN_DATE_TIME
      ,convert(datetime, 
               convert(CHAR(8), l.RESULT_DATE, 112)+ ' ' + CONVERT(CHAR(8), l.RESULT_TIME, 108)
               ) RESULT_DATE_TIME
      ,l.SPECIMEN_SOURCE
      ,l.LAB_LOINC
      ,l.LAB_PX
      ,l.LAB_PX_TYPE
      ,l.RESULT_QUAL
      ,l.RESULT_NUM
      ,l.RESULT_UNIT
      ,datediff(dd,pat.ADMIT_DATE,l.SPECIMEN_DATE) DAYS_SINCE_ADMIT
from #AKI_onsets pat
join [&&dbname].[&&PCORNET_CDM].LAB_RESULT_CM l
on pat.PATID = l.PATID and l.LAB_ORDER_DATE between pat.ADMIT_DATE and pat.DISCHARGE_DATE
order by pat.PATID, pat.ENCOUNTERID, SPECIMEN_DATE_TIME
