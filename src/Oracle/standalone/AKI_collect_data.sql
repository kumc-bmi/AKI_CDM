/*******************************************************************************
 AKI_collect_data.sql is used to collect all relavent clinical observations for
 the AKI cohort collected in AKI_onsets. More details can be found 
 at: https://github.com/kumc-bmi/AKI_CDM
 
 - &&cdm_db_schema will be substituted by corresponding CDM schema
********************************************************************************/

/*Demographic Table*/
create table AKI_DEMO as
select distinct
       pat.PATID
      ,to_char(pat.ENCOUNTERID) ENCOUNTERID
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
left join &&cdm_db_schema.DEMOGRAPHIC demo
on pat.PATID = demo.PATID
left join &&cdm_db_schema.DEATH dth
on pat.PATID = dth.PATID
order by pat.PATID, ENCOUNTERID
;

/*Vital Table*/
create table AKI_VITAL as
select pat.PATID
      ,to_char(pat.ENCOUNTERID) ENCOUNTERID
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
;

/*Procedure Table*/
create table AKI_PX as
select distinct
       pat.PATID
      ,to_char(pat.ENCOUNTERID) ENCOUNTERID
      ,px.PX
      ,px.PX_TYPE
      ,px.PX_SOURCE
      ,px.PX_DATE
      ,round(px.PX_DATE-pat.ADMIT_DATE) DAYS_SINCE_ADMIT
from AKI_onsets pat
left join &&cdm_db_schema.PROCEDURES px
on pat.PATID = px.PATID
where px.PX_DATE between pat.ADMIT_DATE and coalesce(pat.AKI3_ONSET,pat.AKI2_ONSET,pat.AKI1_ONSET,pat.NONAKI_ANCHOR,pat.DISCHARGE_DATE)
order by pat.PATID, ENCOUNTERID, px.PX_DATE desc
;

/*Diagnoses Table (historic)*/
create table AKI_DX as
select pat.PATID
      ,to_char(pat.ENCOUNTERID) ENCOUNTERID
      ,dx.DX
      ,dx.DX_TYPE
      ,dx.DX_SOURCE
      ,dx.PDX
      ,dx.ADMIT_DATE DX_DATE
      ,round(dx.ADMIT_DATE-pat.ADMIT_DATE) DAYS_SINCE_ADMIT
from AKI_onsets pat
join &&cdm_db_schema.DIAGNOSIS dx
on pat.PATID = dx.PATID
where dx.ADMIT_DATE between pat.ADMIT_DATE-365 and pat.ADMIT_DATE-1
order by pat.PATID, ENCOUNTERID, dx.ADMIT_DATE desc
;

/*Lab Table*/
create table AKI_LAB as
select distinct
       pat.PATID
      ,to_char(pat.ENCOUNTERID) ENCOUNTERID
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
      ,round(l.SPECIMEN_DATE-pat.ADMIT_DATE) DAYS_SINCE_ADMIT
from AKI_onsets pat
join &&cdm_db_schema.LAB_RESULT_CM l
on pat.PATID = l.PATID and l.LAB_ORDER_DATE between pat.ADMIT_DATE and least(coalesce(pat.AKI3_ONSET,pat.AKI2_ONSET,pat.AKI1_ONSET,pat.NONAKI_ANCHOR,pat.DISCHARGE_DATE),pat.DISCHARGE_DATE)
order by pat.PATID, ENCOUNTERID, SPECIMEN_DATE_TIME
;

/*Prescribing Table*/
create table AKI_PMED as
select distinct
       pat.PATID
      ,to_char(pat.ENCOUNTERID) ENCOUNTERID
      ,to_date(to_char(trunc(p.RX_ORDER_DATE),'YYYY:MM:DD') || ' ' || to_char(p.RX_ORDER_TIME),
               'YYYY:MM:DD HH24:MI') RX_ORDER_DATE_TIME
      ,p.RX_START_DATE
      ,least(pat.DISCHARGE_DATE,p.RX_END_DATE) RX_END_DATE
      ,p.RX_BASIS
      ,p.RXNORM_CUI
      --,regexp_substr(p.RAW_RX_MED_NAME,'[^\[]+',1,1) RX_MED_NAME
      ,p.RX_QUANTITY
      --,p.RX_QUANTITY_UNIT
      ,p.RX_REFILLS
      ,p.RX_DAYS_SUPPLY
      ,p.RX_FREQUENCY
      ,case when p.RX_DAYS_SUPPLY > 0 and p.RX_QUANTITY is not null then round(p.RX_QUANTITY/p.RX_DAYS_SUPPLY) 
            else null end as RX_QUANTITY_DAILY
      ,round(p.RX_START_DATE-pat.ADMIT_DATE,2) DAYS_SINCE_ADMIT
from AKI_onsets pat
join &&cdm_db_schema.PRESCRIBING p
on pat.PATID = p.PATID
where p.RXNORM_CUI is not null and
      p.RX_START_DATE is not null and
      p.RX_ORDER_DATE is not null and 
      p.RX_ORDER_TIME is not null and
      p.RX_ORDER_DATE between pat.ADMIT_DATE-30 and coalesce(pat.AKI3_ONSET,pat.AKI2_ONSET,pat.AKI1_ONSET,pat.NONAKI_ANCHOR,pat.DISCHARGE_DATE)
order by PATID, ENCOUNTERID, RXNORM_CUI, RX_START_DATE
;


/*Med Admin Table*/
create table AKI_AMED as
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
;

-------------------------------------------------------------------------------
/* eyeball several lines and export the following tables as :
 - AKI_DEMO
 - AKI_VITAL
 - AKI_PX
 - AKI_DX
 - AKI_LAB
 - AKI_PMED
 - AKI_AMED
------------------------------------------------------------------------------------
