/********************************************************************************/
/*@file collect_dx.sql
/*
/*in: AKI_onsets
/*
/*params: [@dblink], &&dbname, &&PCORNET_CDM
/*
/*out: AKI_DX
/*
/*action: query
/********************************************************************************/
select pat.PATID
      ,pat.ENCOUNTERID
      --,dx.ENC_TYPE
      ,dx.DX
      ,dx.DX_TYPE
      ,dx.DX_SOURCE
      ,dx.DX_ORIGIN
      ,dx.PDX
      ,dx.ADMIT_DATE DX_DATE
      ,-datediff(dd,dx.ADMIT_DATE,pat.ADMIT_DATE) DAYS_SINCE_ADMIT
from AKI_onsets pat
join [@dblink].[&&dbname].[&&PCORNET_CDM].DIAGNOSIS dx
on pat.PATID = dx.PATID
where datediff(dd,dx.ADMIT_DATE,pat.ADMIT_DATE) between 1 and 365
order by pat.PATID, pat.ENCOUNTERID, dx.ADMIT_DATE desc


