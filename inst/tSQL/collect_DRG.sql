/********************************************************************************/
/*@file collect_DRG.sql
/*
/*in: AKI_onsets
/*
/*params: &&PCORNET_CDM, @server
/*
/*out: AKI_DRG
/*
/*action: query
/********************************************************************************/
with DRG as (
select distinct
       pat.PATID
      ,pat.ENCOUNTERID
      ,e.DRG
      ,'ADMIT_DRG' DRG_TYPE
      ,e.ADMIT_DATE DRG_DATE
      ,e.DISCHARGE_DATE
      ,e.ENC_TYPE
      ,e.DISCHARGE_DISPOSITION
      ,e.DISCHARGE_STATUS
from AKI_onsets pat
left join [@server].[&&PCORNET_CDM].ENCOUNTER e
on pat.ENCOUNTERID = e.ENCOUNTERID
union all
select distinct
       pat.PATID
      ,pat.ENCOUNTERID
      ,e.DRG
      ,'COMMORB_DRG' DRG_TYPE
      ,e.ADMIT_DATE DRG_DATE
      ,e.DISCHARGE_DATE
      ,e.ENC_TYPE
      ,e.DISCHARGE_DISPOSITION
      ,e.DISCHARGE_STATUS
from AKI_onsets pat
left join [@server].[&&PCORNET_CDM].ENCOUNTER e
on pat.PATID = e.PATID
where datediff(dd,e.ADMIT_DATE,pat.ADMIT_DATE) between 1 and 365
)
select distinct
       PATID
      ,ENCOUNTERID
      ,DRG
      ,DRG_TYPE
      ,DRG_DATE
      ,DISCHARGE_DATE
      ,ENC_TYPE
      ,DISCHARGE_DISPOSITION
      ,DISCHARGE_STATUS
from DRG
order by PATID, ENCOUNTERID, DRG_DATE


