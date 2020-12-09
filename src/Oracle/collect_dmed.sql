/********************************************************************************/
/*@file collect_dmed.sql
/*
/*in: AKI_onsets
/*
/*params: &&cdm_db_schema
/*
/*out: AKI_MED
/*
/*action: query
/********************************************************************************/
select distinct
       pat.PATID
      ,pat.ENCOUNTERID
      ,d.NDC
      ,d.DISPENSE_DATE
      ,d.DISPENSE_SOURCE
      ,d.DISPENSE_SUP
      ,d.DISPENSE_AMT
      ,d.DISPENSE_DOSE_DISP
      ,case when d.DISPENSE_SUP > 0 and d.DISPENSE_AMT is not null then round(d.DISPENSE_AMT/d.DISPENSE_SUP) 
            else null end as RX_QUANTITY_DAILY
      ,round(d.DISPENSE_DATE-pat.ADMIT_DATE,2) DAYS_SINCE_ADMIT
from AKI_onsets pat
join &&cdm_db_schema.DISPENSING d
on pat.PATID = d.PATID
where d.DISPENSE_DATE is not null and
      d.NDC is not null and 
      d.DISPENSE_DATE between pat.ADMIT_DATE-180 and pat.DISCHARGE_DATE+30
order by PATID, DISPENSE_DATE


