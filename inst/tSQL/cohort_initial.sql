/*******************************************************************************/
/*@file create_initial_cohort.sql
/*
/*in: PCORNET_CDM tables 
/*
/*params: &&PCORNET_CDM, @server, &&start_date, &&end_date
/*       
/*out: AKI_Initial
/*
/*action: write
/********************************************************************************/
with age_at_admit as (
select e.ENCOUNTERID
      ,e.PATID
      ,convert(datetime, 
               convert(CHAR(8), e.ADMIT_DATE, 112)+ ' ' + CONVERT(CHAR(8), e.ADMIT_TIME, 108)
               ) ADMIT_DATE_TIME
      ,round((e.ADMIT_DATE-d.BIRTH_DATE)/365.25) age_at_admit
      ,convert(datetime, 
               convert(CHAR(8), e.DISCHARGE_DATE, 112)+ ' ' + CONVERT(CHAR(8), e.DISCHARGE_TIME, 108)
               ) DISCHARGE_DATE_TIME
      ,round(e.DISCHARGE_DATE - e.ADMIT_DATE) LOS
      ,e.ENC_TYPE
      ,e.DISCHARGE_DISPOSITION
      ,e.DISCHARGE_STATUS
      ,e.DRG
      ,e.DRG_TYPE
      ,e.ADMITTING_SOURCE
from [@server].[&&PCORNET_CDM].ENCOUNTER e
join [@server].[&&PCORNET_CDM].DEMOGRAPHIC d
on e.PATID = d.PATID
where e.DISCHARGE_DATE - e.ADMIT_DATE >= 2 and
      e.ENC_TYPE in ('EI','IP','IS') and
      e.ADMIT_DATE between Date &&start_date and Date &&end_date
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
from age_at_admit
where age_at_admit >= 18
into #AKI_Initial



