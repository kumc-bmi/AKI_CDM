/********************************************************************************/
/*@file collect_lab.sql
/*
/*in: AKI_onsets
/*
/*params: &&PCORNET_CDM, @server
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
               convert(CHAR(8), e.SPECIMEN_DATE, 112)+ ' ' + CONVERT(CHAR(8), e.SPECIMEN_TIME, 108)
               ) SPECIMEN_DATE_TIME
      ,convert(datetime, 
               convert(CHAR(8), e.RESULT_DATE, 112)+ ' ' + CONVERT(CHAR(8), e.RESULT_TIME, 108)
               ) RESULT_DATE_TIME
      ,l.SPECIMEN_SOURCE
      ,l.LAB_LOINC
      ,l.LAB_PX
      ,l.LAB_PX_TYPE
      ,l.RESULT_QUAL
      ,l.RESULT_NUM
      ,l.RESULT_UNIT
from AKI_onsets pat
join [@server].[&&PCORNET_CDM].LAB_RESULT_CM l
on pat.ENCOUNTERID = l.ENCOUNTERID
order by pat.PATID, pat.ENCOUNTERID, SPECIMEN_DATE_TIME


