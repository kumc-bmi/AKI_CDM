/********************************************************************************/
/*@file collect_px.sql
/*
/*in: #AKI_onsets
/*
/*params: &&cdm_db_name, &&cdm_db_schema
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
      ,datediff(dd,pat.ADMIT_DATE,px.PX_DATE) DAYS_SINCE_ADMIT
--      ,px.PPX
from #AKI_onsets pat
left join [&&cdm_db_name].[&&cdm_db_schema].PROCEDURES px
on pat.PATID = px.PATID
where px.PX_DATE between pat.ADMIT_DATE and coalesce(pat.AKI3_ONSET,pat.AKI2_ONSET,pat.AKI1_ONSET,pat.NONAKI_ANCHOR,pat.DISCHARGE_DATE)
order by pat.PATID, pat.ENCOUNTERID, px.PX_DATE desc
