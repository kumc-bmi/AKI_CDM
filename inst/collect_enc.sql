/********************************************************************************/
/*@file get_final_cohort.sql
/*
/*in: AKI_onsets
/*
/*params: &&PCORNET_CDM
/*
/*out: AKI_ENC
/*
/*action: query
/********************************************************************************/
select distinct
       pat.PATID
      ,pat.ENCOUNTERID
      ,e.ADMIT_DATE
      ,e.DISCHARGE_DATE
      ,e.ENC_TYPE
      ,e.DISCHARGE_DISPOSITION
      ,e.DISCHARGE_STATUS
      ,e.DRG
--      ,e.DRG_TYPE
      ,e.ADMITTING_SOURCE
from AKI_onsets pat
left join &&PCORNET_CDM.ENCOUNTER e
on pat.ENCOUNTERID = e.ENCOUNTERID
order by pat.PATID, pat.ENCOUNTERID, e.ADMIT_DATE;

