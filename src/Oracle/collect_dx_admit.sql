/********************************************************************************/
/*@file collect_dx_admit.sql
/*
/*in: AKI_onsets
/*
/*params: &&cdm_db_schema
/*
/*out: AKI_DX_ADMIT
/*
/*action: query
/********************************************************************************/
select pat.PATID
      ,pat.ENCOUNTERID
      ,pat.SERUM_CREAT_BASE
      --,dx.ENC_TYPE
      ,dx.DX
      ,dx.DX_TYPE
      ,dx.DX_SOURCE
      --,dx.DX_ORIGIN
      ,dx.PDX
      ,dx.ADMIT_DATE DX_DATE
      ,round(dx.ADMIT_DATE-pat.ADMIT_DATE) DAYS_SINCE_ADMIT
from AKI_onsets pat
join &&cdm_db_schema.DIAGNOSIS dx
on pat.ENCOUNTERID = dx.ENCOUNTERID
where dx.DX_SOURCE = 'AD'
order by pat.PATID, pat.ENCOUNTERID, dx.ADMIT_DATE desc


