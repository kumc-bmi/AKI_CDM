/********************************************************************************/
/*@file collect_med.sql
/*
/*in: #AKI_onsets
/*
/*params: &&dbname, &&PCORNET_CDM
/*
/*out: AKI_MED
/*
/*action: query
/********************************************************************************/
select distinct
       pat.PATID
      ,pat.ENCOUNTERID
      ,convert(datetime, 
               convert(CHAR(8), p.RX_ORDER_DATE, 112)+ ' ' + CONVERT(CHAR(8), p.RX_ORDER_TIME, 108)
               ) RX_ORDER_DATE_TIME
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
      ,datediff(dd,pat.ADMIT_DATE,p.RX_START_DATE) DAYS_SINCE_ADMIT
from #AKI_onsets pat
join [&&dbname].[&&PCORNET_CDM].PRESCRIBING p
on pat.PATID = p.PATID
where p.RXNORM_CUI is not null and 
      p.RX_START_DATE is not null and 
      p.RX_ORDER_DATE is not null and 
      p.RX_ORDER_TIME is not null and
      p.RX_ORDER_DATE between dateadd(day,-30,pat.ADMIT_DATE) and coalesce(pat.AKI3_ONSET,pat.AKI2_ONSET,pat.AKI1_ONSET,pat.NONAKI_ANCHOR,pat.DISCHARGE_DATE)
order by PATID, ENCOUNTERID, RXNORM_CUI, RX_START_DATE
