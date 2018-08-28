/********************************************************************************/
/*@file collect_lab.sql
/*
/*in: AKI_onsets
/*
/*params: &&PCORNET_CDM
/*
/*out: AKI_LAB
/*
/*action: query
/********************************************************************************/
select distinct
       pat.PATID
      ,pat.ENCOUNTERID
      ,l.LAB_ORDER_DATE
      ,to_date(to_char(trunc(l.SPECIMEN_DATE),'YYYY:MM:DD') || ' ' || to_char(l.SPECIMEN_TIME),
               'YYYY:MM:DD HH24:MI') SPECIMEN_DATE_TIME
      ,to_date(to_char(trunc(l.RESULT_DATE),'YYYY:MM:DD') || ' ' || to_char(l.RESULT_TIME),
               'YYYY:MM:DD HH24:MI') RESULT_DATE_TIME
      ,l.SPECIMEN_SOURCE
      ,l.LAB_LOINC
      ,l.LAB_PX
      ,l.LAB_PX_TYPE
      ,l.RESULT_QUAL
      ,l.RESULT_NUM
      ,l.RESULT_UNIT
from AKI_onsets pat
join &&PCORNET_CDM.LAB_RESULT_CM l
on pat.ENCOUNTERID = l.ENCOUNTERID
order by pat.PATID, pat.ENCOUNTERID, SPECIMEN_DATE_TIME


