/*******************************************************************************
 AKI_collect_data.sql is used to collect all relavent clinical observations for
 the AKI cohort collected in AKI_onsets. More details can be found 
 at: https://github.com/kumc-bmi/AKI_CDM
 
 - &&cdm_db_name will be substituted by corresponding database name where CDM data is
 - &&cdm_db_schema will be substituted by corresponding CDM schema
********************************************************************************/

/*Demographic Table*/
select distinct
       pat.PATID
      ,pat.ENCOUNTERID
      ,demo.BIRTH_DATE
      ,(CONVERT(int,CONVERT(char(8),pat.ADMIT_DATE,112))-CONVERT(int,CONVERT(char(8),demo.BIRTH_DATE,112)))/10000 AS AGE
      ,demo.SEX
      ,demo.RACE
      ,demo.HISPANIC
      ,dth.DEATH_DATE
      ,datediff(dd,pat.DISCHARGE_DATE,dth.DEATH_DATE) DDAYS_SINCE_ENC
      ,dth.DEATH_DATE_IMPUTE
      ,dth.DEATH_SOURCE
into #AKI_DEMO
from #AKI_onsets pat
left join [&&cdm_db_name].[&&cdm_db_schema].DEMOGRAPHIC demo
on pat.PATID = demo.PATID
left join [&&cdm_db_name].[&&cdm_db_schema].DEATH dth
on pat.PATID = dth.PATID
order by pat.PATID, pat.ENCOUNTERID
;

/*Vital Table*/
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
into #AKI_VITAL
from #AKI_onsets pat
left join [&&cdm_db_name].[&&cdm_db_schema].VITAL v
on pat.PATID = v.PATID
where v.MEASURE_DATE between dateadd(day,-7,pat.ADMIT_DATE) and coalesce(pat.AKI3_ONSET,pat.AKI2_ONSET,pat.AKI1_ONSET,pat.NONAKI_ANCHOR,pat.DISCHARGE_DATE) and
      coalesce(v.HT, v.WT, v.SYSTOLIC, v.DIASTOLIC, v.ORIGINAL_BMI) is not null
order by PATID, ENCOUNTERID, MEASURE_DATE_TIME
;

/*Procedure Table*/
select distinct
       pat.PATID
      ,pat.ENCOUNTERID
      --,px.ENC_TYPE
      ,px.PX
      ,px.PX_TYPE
      ,px.PX_SOURCE
      ,px.PX_DATE
      ,datediff(dd,pat.ADMIT_DATE,px.PX_DATE) DAYS_SINCE_ADMIT
--      ,px.PPX
into #AKI_PX
from #AKI_onsets pat
left join [&&cdm_db_name].[&&cdm_db_schema].PROCEDURES px
on pat.PATID = px.PATID
where px.PX_DATE between pat.ADMIT_DATE and coalesce(pat.AKI3_ONSET,pat.AKI2_ONSET,pat.AKI1_ONSET,pat.NONAKI_ANCHOR,pat.DISCHARGE_DATE)
order by pat.PATID, pat.ENCOUNTERID, px.PX_DATE desc


/*Diagnoses Table (historic)*/
select pat.PATID
      ,pat.ENCOUNTERID
      --,dx.ENC_TYPE
      ,dx.DX
      ,dx.DX_TYPE
      ,dx.DX_SOURCE
      ,dx.DX_ORIGIN
      ,dx.PDX
      ,dx.ADMIT_DATE DX_DATE
      ,datediff(dd,pat.ADMIT_DATE,dx.ADMIT_DATE) as DAYS_SINCE_ADMIT
into #AKI_DX
from #AKI_onsets pat
join [&&cdm_db_name].[&&cdm_db_schema].DIAGNOSIS dx
on pat.PATID = dx.PATID
where datediff(dd,dx.ADMIT_DATE,pat.ADMIT_DATE) between 1 and 365
order by pat.PATID, pat.ENCOUNTERID, dx.ADMIT_DATE desc


/*Lab Table*/
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
into #AKI_LAB
from #AKI_onsets pat
join [&&cdm_db_name].[&&cdm_db_schema].LAB_RESULT_CM l
on pat.PATID = l.PATID and l.LAB_ORDER_DATE between pat.ADMIT_DATE and coalesce(pat.AKI3_ONSET,pat.AKI2_ONSET,pat.AKI1_ONSET,pat.NONAKI_ANCHOR,pat.DISCHARGE_DATE)
order by pat.PATID, pat.ENCOUNTERID, SPECIMEN_DATE_TIME
;

/*Prescribing Table*/
select distinct
       pat.PATID
      ,pat.ENCOUNTERID
      ,convert(datetime, 
               convert(CHAR(8), p.RX_ORDER_DATE, 112)+ ' ' + CONVERT(CHAR(8), p.RX_ORDER_TIME, 108)
               ) RX_ORDER_DATE_TIME
      ,p.RX_START_DATE
      ,case when pat.DISCHARGE_DATE < p.RX_END_DATE THEN pat.DISCHARGE_DATE 
            else p.RX_END_DATE 
       end as RX_END_DATE
      ,p.RX_BASIS
      ,p.RXNORM_CUI
      --,regexp_substr(p.RAW_RX_MED_NAME,'[^\[]+',1,1) RX_MED_NAME
      ,p.RX_QUANTITY
      --,p.RX_QUANTITY_UNIT
      ,p.RX_REFILLS
      ,p.RX_DAYS_SUPPLY
      ,p.RX_FREQUENCY
      ,case when p.RX_DAYS_SUPPLY is not null and p.RX_DAYS_SUPPLY is not null then round(p.RX_QUANTITY/p.RX_DAYS_SUPPLY,0) 
            else null 
	     end as RX_QUANTITY_DAILY
      ,datediff(dd,pat.ADMIT_DATE,p.RX_START_DATE) DAYS_SINCE_ADMIT
into #AKI_PMED
from #AKI_onsets pat
join [&&cdm_db_name].[&&cdm_db_schema].PRESCRIBING p
on pat.PATID = p.PATID
where p.RXNORM_CUI is not null and 
      p.RX_START_DATE is not null and 
      p.RX_ORDER_DATE is not null and 
      p.RX_ORDER_TIME is not null and
      p.RX_ORDER_DATE between dateadd(day,-30,pat.ADMIT_DATE) and coalesce(pat.AKI3_ONSET,pat.AKI2_ONSET,pat.AKI1_ONSET,pat.NONAKI_ANCHOR,pat.DISCHARGE_DATE)
order by PATID, ENCOUNTERID, RXNORM_CUI, RX_START_DATE
;

/*Dispensing Table*/
-- Note: for sites don't populate this table, please skip
select distinct
       pat.PATID
      ,pat.ENCOUNTERID
      ,d.PRESCRIBINGID
      ,d.DISPENSING_DATE
      ,d.NDC
      ,d.DISPENSE_SOURCE
      ,d.DISPENSE_SUP
      ,d.DISPENSE_AMT
      ,d.DISPENSE_DOSE_DISP
      ,d.DISPENSE_DOSE_DISP_UNIT
      ,d.DISPENSE_ROUTE
      ,datediff(dd,pat.ADMIT_DATE,d.DISPENSING_DATE) DAYS_SINCE_ADMIT
into #AKI_DMED
from #AKI_onsets pat
join [&&cdm_db_name].[&&cdm_db_schema].DISPENSING p
on pat.PATID = d.PATID
where d.NDC is not null and
      d.DISPENSING_DATE between dateadd(day,-30,pat.ADMIT_DATE) and coalesce(pat.AKI3_ONSET,pat.AKI2_ONSET,pat.AKI1_ONSET,pat.NONAKI_ANCHOR,pat.DISCHARGE_DATE)
;


/*Med Admin Table*/
-- Note: for sites don't populate this table, please skip
select distinct
       pat.PATID
      ,pat.ENCOUNTERID
      ,convert(datetime, 
               convert(CHAR(8), m.MEDADMIN_START_DATE, 112)+ ' ' + CONVERT(CHAR(8), m.MEDADMIN_START_TIME, 108)
               ) MEDADMIN_START_DATE_TIME
      ,convert(datetime, 
               convert(CHAR(8), m.MEDADMIN_STOP_DATE, 112)+ ' ' + CONVERT(CHAR(8), m.MEDADMIN_STOP_TIME, 108)
               ) MEDADMIN_STOP_DATE_TIME
      ,m.MEDADMIN_TYPE
      ,m.MEDADMIN_CODE
      --,m.RAW_MEDADMIN_MED_NAME
      ,m.MEDADMIN_DOSE_ADMIN
      --,m.MEDADMIN_DOSE_ADMIN_UNIT
      ,m.MEDADMIN_ROUTE
      ,m.MEDADMIN_SOURCE
      ,datediff(dd,pat.ADMIT_DATE,m.MEDADMIN_START_DATE) DAYS_SINCE_ADMIT
into #AKI_AMED
from #AKI_onsets pat
join [&&cdm_db_name].[&&cdm_db_schema].MED_ADMIN m
on pat.PATID = m.PATID
where m.MEDADMIN_CODE is not null and
      m.MEDADMIN_START_DATE is not null and
      --m.MEDADMIN_START_TIME is not null and 
      m.MEDADMIN_STOP_DATE is not null and
      --m.MEDADMIN_STOP_TIME is null and
      m.MEDADMIN_START_DATE between dateadd(day,-30,pat.ADMIT_DATE) and coalesce(pat.AKI3_ONSET,pat.AKI2_ONSET,pat.AKI1_ONSET,pat.NONAKI_ANCHOR,pat.DISCHARGE_DATE)
;

-------------------------------------------------------------------------------
/* Eyeball several lines and export the following tables as .csv files. Please 
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
