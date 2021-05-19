/*******************************************************************************
 AKI_collect_data.sql is used to collect all relavent clinical observations for
 the AKI cohort collected in AKI_onsets. More details can be found 
 at: https://github.com/kumc-bmi/AKI_CDM
 
 - &&cdm_db_schema will be substituted by corresponding CDM schema
********************************************************************************/

/*Demographic Table*/
-- calculate age in years: https://stackoverflow.com/questions/17833176/postgresql-days-months-years-between-two-dates

set cdm_db_schema = 'PCORNET_CDM.CDM_C009R020';
Set ENCOUNTER = CONCAT($cdm_db_schema,'.ENCOUNTER');
Set DEMOGRAPHIC = CONCAT($cdm_db_schema,'.DEMOGRAPHIC');
Set LAB_RESULT_CM = CONCAT($cdm_db_schema,'.LAB_RESULT_CM');
Set DIAGNOSIS = CONCAT($cdm_db_schema,'.DIAGNOSIS');
Set PROCEDURES = CONCAT($cdm_db_schema,'.PROCEDURES');
Set DEATH = CONCAT($cdm_db_schema,'.DEATH');
Set DISPENSING = CONCAT($cdm_db_schema,'.DISPENSING');
Set MED_ADMIN = CONCAT($cdm_db_schema,'.MED_ADMIN');
Set PRESCRIBING = CONCAT($cdm_db_schema,'.PRESCRIBING');
Set VITAL = CONCAT($cdm_db_schema,'.VITAL');
Set start_date = '2010-01-01';
Set end_date = '2019-12-31';
show variables;

create table AKI_DEMO as
select distinct
       pat.PATID
      ,to_char(pat.ENCOUNTERID) ENCOUNTERID
      ,demo.BIRTH_DATE
      ,YEAR(pat.ADMIT_DATE::date) - YEAR(demo.BIRTH_DATE::date) AGE
      ,demo.SEX
      ,demo.RACE
      ,demo.HISPANIC
      ,dth.DEATH_DATE
      ,DAY(dth.DEATH_DATE::date) - DAY(pat.DISCHARGE_DATE::date) DAYS_SINCE_ENC
      ,dth.DEATH_DATE_IMPUTE
      ,dth.DEATH_SOURCE
from AKI_onsets pat
left join identifier($DEMOGRAPHIC) demo
on pat.PATID = demo.PATID
left join identifier($DEATH) dth
on pat.PATID = dth.PATID
order by pat.PATID, ENCOUNTERID
;

/*Vital Table*/
-- add/substract days to a date: https://stackoverflow.com/questions/46079791/subtracting-1-day-from-a-timestamp-date
create table AKI_VITAL as
select pat.PATID
      ,to_char(pat.ENCOUNTERID) ENCOUNTERID
      ,timestamp_ntz_from_parts(v.MEASURE_DATE::date , v.MEASURE_TIME::time) MEASURE_DATE_TIME
      ,v.HT
      ,v.WT
      ,v.SYSTOLIC
      ,v.DIASTOLIC
      ,v.ORIGINAL_BMI
      ,v.SMOKING
      ,v.TOBACCO
      ,v.TOBACCO_TYPE
      ,DAY(v.MEASURE_DATE::date) - DAY(pat.ADMIT_DATE::date) DAYS_SINCE_ADMIT
from AKI_onsets pat
left join identifier($VITAL) v
on pat.PATID = v.PATID
where v.MEASURE_DATE between (pat.ADMIT_DATE - INTERVAL '7 DAYS') and coalesce(pat.AKI3_ONSET,pat.AKI2_ONSET,pat.AKI1_ONSET,pat.NONAKI_ANCHOR,pat.DISCHARGE_DATE) and
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
      ,DAY(px.PX_DATE::date) - DAY(pat.ADMIT_DATE::date) DAYS_SINCE_ADMIT
from AKI_onsets pat
left join identifier($PROCEDURES) px
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
      ,DAY(dx.ADMIT_DATE::date) - DAY(pat.ADMIT_DATE::date) DAYS_SINCE_ADMIT
from AKI_onsets pat
join identifier($DIAGNOSIS)dx
on pat.PATID = dx.PATID
where dx.ADMIT_DATE between (pat.ADMIT_DATE - INTERVAL '365 DAYS') and (pat.ADMIT_DATE - INTERVAL '1 DAY')
order by pat.PATID, ENCOUNTERID, dx.ADMIT_DATE desc
;

/*Lab Table*/
create table AKI_LAB as
select distinct
       pat.PATID
      ,to_char(pat.ENCOUNTERID) ENCOUNTERID
      ,l.LAB_ORDER_DATE
      ,timestamp_ntz_from_parts(l.SPECIMEN_DATE::date, l.SPECIMEN_TIME::time) SPECIMEN_DATE_TIME
      ,timestamp_ntz_from_parts(l.RESULT_DATE::date, l.RESULT_TIME::time) RESULT_DATE_TIME
      ,l.SPECIMEN_SOURCE
      ,l.LAB_LOINC
      ,l.LAB_PX
      ,l.LAB_PX_TYPE
      ,l.RESULT_QUAL
      ,l.RESULT_NUM
      ,l.RESULT_UNIT
      ,DAY(l.SPECIMEN_DATE::date) - DAY(pat.ADMIT_DATE::date) DAYS_SINCE_ADMIT
from AKI_onsets pat
join identifier($LAB_RESULT_CM) l
on pat.PATID = l.PATID and l.LAB_ORDER_DATE between pat.ADMIT_DATE and least(coalesce(pat.AKI3_ONSET,pat.AKI2_ONSET,pat.AKI1_ONSET,pat.NONAKI_ANCHOR,pat.DISCHARGE_DATE),pat.DISCHARGE_DATE)
order by pat.PATID, ENCOUNTERID, SPECIMEN_DATE_TIME
;

/*Prescribing Table*/
create table AKI_PMED as
select distinct
       pat.PATID
      ,to_char(pat.ENCOUNTERID) ENCOUNTERID
      ,timestamp_ntz_from_parts(p.RX_ORDER_DATE::date, p.RX_ORDER_TIME::time) RX_ORDER_DATE_TIME
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
      ,DAY(p.RX_START_DATE::date) - DAY(pat.ADMIT_DATE::date) DAYS_SINCE_ADMIT
from AKI_onsets pat
join identifier($PRESCRIBING) p
on pat.PATID = p.PATID
where p.RXNORM_CUI is not null and
      p.RX_START_DATE is not null and
      p.RX_ORDER_DATE is not null and 
      p.RX_ORDER_TIME is not null and
      p.RX_ORDER_DATE between (pat.ADMIT_DATE - INTERVAL '30 DAYS') and coalesce(pat.AKI3_ONSET,pat.AKI2_ONSET,pat.AKI1_ONSET,pat.NONAKI_ANCHOR,pat.DISCHARGE_DATE)
order by PATID, ENCOUNTERID, RXNORM_CUI, RX_START_DATE
;

/*Dispensing Table*/
-- Note: for sites don't populate this table, please skip
create table AKI_DMED as
select distinct
       pat.PATID
      ,pat.ENCOUNTERID
      ,d.PRESCRIBINGID
      ,d.DISPENSE_DATE
      ,d.NDC
      ,d.DISPENSE_SOURCE
      ,d.DISPENSE_SUP
      ,d.DISPENSE_AMT
      ,d.DISPENSE_DOSE_DISP
      ,d.DISPENSE_DOSE_DISP_UNIT
      ,d.DISPENSE_ROUTE
      ,DAY(d.DISPENSE_DATE::date) - DAY(pat.ADMIT_DATE::date) DAYS_SINCE_ADMIT
from AKI_onsets pat
join identifier($DISPENSING) d
on pat.PATID = d.PATID
where d.NDC is not null and
      d.DISPENSE_DATE between (pat.ADMIT_DATE - INTERVAL '30 DAYS') and coalesce(pat.AKI3_ONSET,pat.AKI2_ONSET,pat.AKI1_ONSET,pat.NONAKI_ANCHOR,pat.DISCHARGE_DATE)
;


/*Med Admin Table*/
-- Note: for sites don't populate this table, please skip
create table AKI_AMED as
select distinct
       pat.PATID
      ,pat.ENCOUNTERID
      ,timestamp_ntz_from_parts(m.MEDADMIN_START_DATE::date, m.MEDADMIN_START_TIME::time) MEDADMIN_START_DATE_TIME
      ,timestamp_ntz_from_parts(m.MEDADMIN_STOP_DATE::date, m.MEDADMIN_STOP_TIME::time) MEDADMIN_STOP_DATE_TIME
      ,m.MEDADMIN_TYPE
      ,m.MEDADMIN_CODE
      --,m.RAW_MEDADMIN_MED_NAME
      ,m.MEDADMIN_DOSE_ADMIN
      --,m.MEDADMIN_DOSE_ADMIN_UNIT
      ,m.MEDADMIN_ROUTE
      ,m.MEDADMIN_SOURCE
      ,DAY(m.MEDADMIN_START_DATE::date) - DAY(pat.ADMIT_DATE::date) DAYS_SINCE_ADMIT
from AKI_onsets pat
join identifier($MED_ADMIN) m
on pat.PATID = m.PATID
where m.MEDADMIN_CODE is not null and
      m.MEDADMIN_START_DATE is not null and
      --m.MEDADMIN_START_TIME is not null and 
      m.MEDADMIN_STOP_DATE is not null and
      --m.MEDADMIN_STOP_TIME is null and
      m.MEDADMIN_START_DATE between (pat.ADMIT_DATE - INTERVAL '30 DAYS') and coalesce(pat.AKI3_ONSET,pat.AKI2_ONSET,pat.AKI1_ONSET,pat.NONAKI_ANCHOR,pat.DISCHARGE_DATE)
;

-------------------------------------------------------------------------------
/* eyeball several lines and export the following tables as .csv files. Please 
   skip the tables that are not populated. 
   
 - AKI_DEMO
 - AKI_VITAL
 - AKI_PX
 - AKI_DX
 - AKI_LAB
 - AKI_PMED 
 - AKI_AMED
 - AKI_DMED
------------------------------------------------------------------------------------