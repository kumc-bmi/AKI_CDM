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
where px.PX_DATE between dateadd(day,-30,pat.ADMIT_DATE) and
                         pat.DISCHARGE_DATE
order by pat.PATID, pat.ENCOUNTERID, px.PX_DATE desc



