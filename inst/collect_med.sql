/********************************************************************************/
/*@file collect_med.sql
/*
/*in: AKI_onsets
/*
/*params: &&PCORNET_CDM
/*
/*out: AKI_MED
/*
/*action: query
/********************************************************************************/
select distinct
       pat.PATID
      ,pat.ENCOUNTERID
      ,to_date(to_char(trunc(p.RX_ORDER_DATE),'YYYY:MM:DD') || ' ' || to_char(p.RX_ORDER_TIME),
               'YYYY:MM:DD HH24:MI') RX_ORDER_DATE_TIME
      ,p.RX_START_DATE
      ,(p.RX_START_DATE+p.RX_DAYS_SUPPLY) RX_END_DATE_ADJ
      ,p.RX_END_DATE
      ,p.RX_BASIS
      ,p.RXNORM_CUI
      --,regexp_substr(p.RAW_RX_MED_NAME,'[^\[]+',1,1) RX_MED_NAME
      ,p.RX_QUANTITY
      ,p.RX_QUANTITY_UNIT
      ,p.RX_REFILLS
      ,p.RX_DAYS_SUPPLY
      ,p.RX_FREQUENCY
      ,round(p.RX_QUANTITY/p.RX_DAYS_SUPPLY) RX_QUANTITY_DAILY
from AKI_onsets pat
join &&PCORNET_CDM.PRESCRIBING p
on pat.ENCOUNTERID = p.ENCOUNTERID
where p.RXNORM_CUI is not null
order by PATID, ENCOUNTERID, RXNORM_CUI, RX_ORDER_DATE_TIME


