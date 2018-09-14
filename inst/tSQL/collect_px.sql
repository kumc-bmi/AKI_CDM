/********************************************************************************/
/*@file collect_px.sql
/*
/*in: AKI_onsets
/*
/*params: [@dblink], &&dbname, &&PCORNET_CDM
/*
/*out: AKI_PX
/*
/*action: query
/********************************************************************************/
select distinct
       pat.PATID
      ,pat.ENCOUNTERID
      --,px.ENC_TYPE
      ,px.PX
      ,px.PX_TYPE
      ,px.PX_SOURCE
      ,px.PX_DATE
      ,-datediff(dd,pat.ADMIT_DATE,px.PX_DATE) DAYS_SINCE_ADMIT
--      ,px.PPX
from AKI_onsets pat
left join [@dblink].[&&dbname].[&&PCORNET_CDM].PROCEDURES px
on pat.PATID = px.PATID
where datediff(dd,px.PX_DATE,pat.ADMIT_DATE) between 1 and 365 
order by pat.PATID, pat.ENCOUNTERID, px.PX_DATE desc



