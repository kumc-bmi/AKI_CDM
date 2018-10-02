/*******************************************************************************/
/*@file cohort_initial.sql
/*
/*in: PCORNET_CDM tables 
/*
/*params: &&dbname, &&PCORNET_CDM, &&start_date, &&end_date
/*       
/*out: AKI_Initial
/*
/*action: write
/********************************************************************************/
with age_at_admit as (
select e.ENCOUNTERID
      ,e.PATID
      ,convert(datetime,convert(CHAR(8), e.ADMIT_DATE, 112)+ ' ' + CONVERT(CHAR(8), e.ADMIT_TIME, 108)) ADMIT_DATE_TIME
      ,(CONVERT(int,CONVERT(char(8),e.ADMIT_DATE,112))-CONVERT(int,CONVERT(char(8),d.BIRTH_DATE,112)))/10000 AS age_at_admit
      ,convert(datetime,convert(CHAR(8), e.DISCHARGE_DATE, 112)+ ' ' + CONVERT(CHAR(8), e.DISCHARGE_TIME, 108)) DISCHARGE_DATE_TIME
      ,round(datediff(dd,e.ADMIT_DATE,e.DISCHARGE_DATE),0) LOS
      ,e.ENC_TYPE
      ,e.DISCHARGE_DISPOSITION
      ,e.DISCHARGE_STATUS
      ,e.DRG
      ,e.DRG_TYPE
      ,e.ADMITTING_SOURCE
from [&&dbname].[&&PCORNET_CDM].ENCOUNTER e
join [&&dbname].[&&PCORNET_CDM].DEMOGRAPHIC d
on e.PATID = d.PATID
where datediff(dd,e.ADMIT_DATE,e.DISCHARGE_DATE) >= 2 and
      e.ENC_TYPE in ('EI','IP','IS') and
      e.ADMIT_DATE between &&start_date and &&end_date
)

select ENCOUNTERID
      ,PATID
      ,age_at_admit
      ,ADMIT_DATE_TIME
      ,DISCHARGE_DATE_TIME
      ,los
      ,ENC_TYPE
      ,DISCHARGE_DISPOSITION
      ,DISCHARGE_STATUS
      ,DRG
      ,DRG_TYPE
      ,ADMITTING_SOURCE
into #AKI_Initial
from age_at_admit
where age_at_admit >= 18



