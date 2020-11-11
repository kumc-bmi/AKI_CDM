/********************************************************************************/
/*@file collect_med_admin.sql
/*
/*in: AKI_onsets
/*
/*params: &&cdm_db_schema
/*
/*out: AKI_MED
/*
/*action: query
/********************************************************************************/
select distinct
       pat.PATID
      ,pat.ENCOUNTERID
      ,to_date(to_char(trunc(m.MEDADMIN_START_DATE),'YYYY:MM:DD') || ' ' || coalesce(to_char(m.MEDADMIN_START_TIME),'00:00'),
               'YYYY:MM:DD HH24:MI') MEDADMIN_START_DATE_TIME
      ,to_date(to_char(trunc(m.MEDADMIN_STOP_DATE),'YYYY:MM:DD') || ' ' || coalesce(to_char(m.MEDADMIN_STOP_TIME),'00:00'),
               'YYYY:MM:DD HH24:MI') MEDADMIN_STOP_DATE_TIME
      ,m.MEDADMIN_TYPE
      ,m.MEDADMIN_CODE
      --,m.RAW_MEDADMIN_MED_NAME
      ,m.MEDADMIN_DOSE_ADMIN
      --,m.MEDADMIN_DOSE_ADMIN_UNIT
      ,m.MEDADMIN_ROUTE
      ,m.MEDADMIN_SOURCE
      ,round(m.MEDADMIN_START_DATE-pat.ADMIT_DATE,2) DAYS_SINCE_ADMIT
from AKI_onsets pat
join &&cdm_db_schema.MED_ADMIN m
on pat.PATID = m.PATID
where m.MEDADMIN_CODE is not null and
      m.MEDADMIN_START_DATE is not null and
      --m.MEDADMIN_START_TIME is not null and 
      m.MEDADMIN_STOP_DATE is not null and
      --m.MEDADMIN_STOP_TIME is null and
      m.MEDADMIN_START_DATE between pat.ADMIT_DATE-30 and coalesce(pat.AKI3_ONSET,pat.AKI2_ONSET,pat.AKI1_ONSET,pat.NONAKI_ANCHOR,pat.DISCHARGE_DATE)


