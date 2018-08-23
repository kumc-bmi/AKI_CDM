/*******************************************************************************/
/*@file collect_SCr.sql
/*
/*in: AKI_Initial
/*
/*params: &&PCORNET_CDM
/*       
/*out: AKI_Scr_eGFR
/*
/*action: write
/********************************************************************************/
create table AKI_Scr_eGFR as
with Scr_all as (
select l.PATID
      ,l.ENCOUNTERID
      ,avg(l.RESULT_NUM) RESULT_NUM 
      ,l.LAB_ORDER_DATE
      ,l.SPECIMEN_DATE
      ,l.SPECIMEN_TIME
      ,l.RESULT_DATE
      ,l.RESULT_TIME
from &&PCORNET_CDM.LAB_RESULT_CM l
where UPPER(l.LAB_NAME) = 'CREATININE' and 
      UPPER(l.RESULT_UNIT) = 'MG/DL' and
      l.SPECIMEN_SOURCE <> 'URINE' and  /*only serum creatinine*/
      l.RESULT_NUM > 0 and /*value 0 could exist*/
      exists (select 1 from AKI_Initial init
              where init.PATID = l.PATID)
group by l.PATID,l.ENCOUNTERID,l.LAB_ORDER_DATE,
         l.SPECIMEN_DATE,l.SPECIMEN_TIME,l.RESULT_DATE,l.RESULT_TIME
)
    ,Scr_w_age as (
select sa.PATID
      ,sa.ENCOUNTERID
      ,round((sa.LAB_ORDER_DATE - d.BIRTH_DATE)/365.25) AS age_at_Scr
      ,case when d.SEX = 'F' then 1 else 0 end as female_ind 
      ,case when d.RACE = '03' then 1 else 0 end as race_aa_ind /*03=Black or African American*/
      ,sa.RESULT_NUM
      ,sa.LAB_ORDER_DATE
      ,sa.SPECIMEN_DATE
      ,sa.SPECIMEN_TIME
      ,sa.RESULT_DATE
      ,sa.RESULT_TIME
from Scr_all sa
join &&PCORNET_CDM.DEMOGRAPHIC d
on sa.PATID = d.PATID
)
    ,All_Scr_eGFR (
select distinct PATID
      ,ENCOUNTERID
      ,RESULT_NUM SERUM_CREAT
      ,cast(175*power(RESULT_NUM,-1.154)*power(age_at_Scr,-0.203)*(0.742*female_ind+(1-female_ind))*(1.212*race_aa_ind+(1-race_aa_ind)) as BINARY_FLOAT) eGFR 
      ,LAB_ORDER_DATE
      ,to_date(to_char(SPECIMEN_DATE,'YYYY:MM:DD') || ' ' || to_char(SPECIMEN_TIME),
               'YYYY:MM:DD HH24:MI') SPECIMEN_DATE_TIME
      ,to_date(to_char(RESULT_DATE,'YYYY:MM:DD') || ' ' || to_char(RESULT_TIME),
               'YYYY:MM:DD HH24:MI') RESULT_DATE_TIME
      ,dense_rank() over (partition by ENCOUNTERID order by RESULT_DATE, RESULT_TIME) rn
from Scr_w_age
where age_at_Scr >= 18
)
select scr.* from All_Scr_eGFR scr
where exists (select 1 from AKI_Initial aki
              where scr.ENCOUNTERID = aki.ENCOUNTERID);
              