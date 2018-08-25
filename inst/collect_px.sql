/********************************************************************************/
/*@file collect_px.sql
/*
/*in: AKI_onsets
/*
/*params: &&PCORNET_CDM
/*
/*out: AKI_PX
/*
/*action: query
/********************************************************************************/
select distinct
       pat.PATID
      ,pat.ENCOUNTERID
      ,px.ENC_TYPE
      ,px.PX_DATE
      ,round(pat.ADMIT_DATE - px.PX_DATE) PX_DAYS_PRIOR
      ,px.PX
      ,px.PX_TYPE
      ,px.PX_SOURCE
--      ,px.PPX
from AKI_onsets pat
left join &&PCORNET_CDM.PROCEDURES px
on pat.PATID = px.PATID
where px.PX_DATE < pat.ADMIT_DATE
order by pat.PATID, pat.ENCOUNTERID, px.PX_DATE desc



