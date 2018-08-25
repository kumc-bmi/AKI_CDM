/********************************************************************************/
/*@file collect_dx.sql
/*
/*in: AKI_onsets
/*
/*params: &&PCORNET_CDM
/*
/*out: AKI_DX
/*
/*action: query
/********************************************************************************/
select pat.PATID
      ,pat.ENCOUNTERID
      ,dx.ENC_TYPE
      ,dx.ADMIT_DATE
      ,round(pat.ADMIT_DATE - dx.ADMIT_DATE) DX_DAYS_PRIOR
      ,dx.DX
      ,dx.DX_TYPE
      ,dx.DX_SOURCE
      ,dx.DX_ORIGIN
      ,dx.PDX
from AKI_onsets pat
join &&PCORNET_CDM.DIAGNOSIS dx
on pat.PATID = dx.PATID
where dx.ADMIT_DATE < pat.ADMIT_DATE
order by pat.PATID, pat.ENCOUNTERID, dx.ADMIT_DATE desc


