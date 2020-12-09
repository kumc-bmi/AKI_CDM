/********************************************************************************/
/*@file collect_pmed.sql
/*
/*in: AKI_onsets
/*
/*params: &&cdm_db_name, &&cdm_db_schema
/*
/*out: AKI_MED
/*
/*action: query
/********************************************************************************/
select distinct
       pat.PATID
      ,pat.ENCOUNTERID
      ,to_date(to_char(trunc(p.RX_ORDER_DATE),'YYYY:MM:DD') || ' ' || coalesce(to_char(p.RX_ORDER_TIME),'00:00'),
               'YYYY:MM:DD HH24:MI') RX_ORDER_DATE_TIME
      ,p.RX_START_DATE
      ,case when pat.DISCHARGE_DATE < p.RX_END_DATE THEN pat.DISCHARGE_DATE 
            else p.RX_END_DATE 
       end as RX_END_DATE
      ,p.RX_BASIS
      ,p.RXNORM_CUI
      --,regexp_substr(p.RAW_RX_MED_NAME,'[^\[]+',1,1) RX_MED_NAME
      ,p.RX_QUANTITY
      --,p.RX_QUANTITY_UNIT
      ,p.RX_REFILLS
      ,p.RX_DAYS_SUPPLY
      ,p.RX_FREQUENCY
      ,case when p.RX_DAYS_SUPPLY is not null and p.RX_DAYS_SUPPLY is not null then round(p.RX_QUANTITY/p.RX_DAYS_SUPPLY,0) 
            else null 
	     end as RX_QUANTITY_DAILY
	     ,round(p.RX_START_DATE-pat.ADMIT_DATE,2) DAYS_SINCE_ADMIT
from AKI_onsets pat
join &&cdm_db_schema.PRESCRIBING p
on pat.PATID = p.PATID
where p.RXNORM_CUI is not null and 
      p.RX_START_DATE is not null and 
      p.RX_ORDER_DATE is not null and 
      p.RX_ORDER_TIME is not null and
      p.RX_ORDER_DATE between pat.ADMIT_DATE-30 and coalesce(pat.AKI3_ONSET,pat.AKI2_ONSET,pat.AKI1_ONSET,pat.NONAKI_ANCHOR,pat.DISCHARGE_DATE)
order by PATID, ENCOUNTERID, RXNORM_CUI, RX_START_DATE
