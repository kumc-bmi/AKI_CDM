/*******************************************************************************/
/*@file collect_all_SCr.sql
/*
/*ref: https://github.com/kumc-bmi/h2p-mapping/blob/master/Oracle/lab_loinc_mapping.csv
/*
/*in: #AKI_Initial
/*
/*params: &&cdm_db_name, &&cdm_db_schema
/*       
/*out: #All_Scr_eGFR
/*
/*action: write
/********************************************************************************/
with Scr_all as (
select l.PATID
      ,l.ENCOUNTERID
      ,avg(l.RESULT_NUM) RESULT_NUM 
      ,l.LAB_ORDER_DATE
      ,l.SPECIMEN_DATE
      ,l.SPECIMEN_TIME
      ,l.RESULT_DATE
      ,l.RESULT_TIME
from [&&cdm_db_name].[&&cdm_db_schema].LAB_RESULT_CM l
where l.LAB_LOINC in ('2160-0','38483-4','14682-9','21232-4','35203-9','44784-7','59826-8') and 
      UPPER(l.RESULT_UNIT) = 'MG/DL' and
      l.SPECIMEN_SOURCE <> 'URINE' and  /*only serum creatinine*/
      l.RESULT_NUM > 0 and /*value 0 could exist*/
      exists (select 1 from #AKI_Initial init
              where init.PATID = l.PATID)
group by l.PATID,l.ENCOUNTERID,l.LAB_ORDER_DATE,
         l.SPECIMEN_DATE,l.SPECIMEN_TIME,l.RESULT_DATE,l.RESULT_TIME
)
    ,Scr_w_age as (
select distinct
       sa.PATID
      ,sa.ENCOUNTERID
      ,(CONVERT(int,CONVERT(char(8),sa.LAB_ORDER_DATE,112))-CONVERT(int,CONVERT(char(8),d.BIRTH_DATE,112)))/10000 AS age_at_Scr
      ,case when d.SEX = 'F' then 1 else 0 end as female_ind 
      ,case when d.RACE = '03' then 1 else 0 end as race_aa_ind /*03=Black or African American*/
      ,sa.RESULT_NUM
      ,sa.LAB_ORDER_DATE
      ,sa.SPECIMEN_DATE
      ,sa.SPECIMEN_TIME
      ,sa.RESULT_DATE
      ,sa.RESULT_TIME
from Scr_all sa
join [&&cdm_db_name].[&&cdm_db_schema].DEMOGRAPHIC d
on sa.PATID = d.PATID
)
select PATID
      ,ENCOUNTERID
      ,RESULT_NUM SERUM_CREAT
      ,cast(175*round(power(RESULT_NUM,-1.154),2)*round(power(convert(decimal(8,3),age_at_Scr),-0.203),2)*(0.742*female_ind+(1-female_ind))*(1.212*race_aa_ind+(1-race_aa_ind)) as FLOAT) eGFR
      ,LAB_ORDER_DATE
      ,convert(datetime, 
               convert(CHAR(8), SPECIMEN_DATE, 112)+ ' ' + CONVERT(CHAR(8), SPECIMEN_TIME, 108)
               ) SPECIMEN_DATE_TIME
      ,convert(datetime, 
               convert(CHAR(8), RESULT_DATE, 112)+ ' ' + CONVERT(CHAR(8), RESULT_TIME, 108)
               ) RESULT_DATE_TIME
into #All_Scr_eGFR
from Scr_w_age
where age_at_Scr >= 18
