/********************************************************************************/
/*@file collect_dx.sql
/*
/*in: AKI_onsets
/*
/*params: @dbname, &&PCORNET_CDM
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
      ,round(dx.ADMIT_DATE-pat.ADMIT_DATE) DAYS_SINCE_ADMIT
from AKI_onsets pat
join &&PCORNET_CDM.DIAGNOSIS@dbname dx
on pat.PATID = dx.PATID
where dx.ADMIT_DATE between pat.ADMIT_DATE-365 and
                            pat.ADMIT_DATE-1
order by pat.PATID, pat.ENCOUNTERID, dx.ADMIT_DATE desc


