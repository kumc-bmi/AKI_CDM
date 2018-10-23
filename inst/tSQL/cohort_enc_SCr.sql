/*******************************************************************************/
/*@file cohort_enc_SCr.sql
/*
/*in: #All_Scr_eGFR, #AKI_Initial
/*       
/*out: #AKI_Scr_eGFR
/*
/*action: write
/********************************************************************************/
select scr.*  
      ,dense_rank() over (partition by PATID order by LAB_ORDER_DATE,SPECIMEN_DATE_TIME) rn
into #AKI_Scr_eGFR
from #All_Scr_eGFR scr
where exists (select 1 from #AKI_Initial aki where scr.ENCOUNTERID = aki.ENCOUNTERID) or
      exists (select 1 from #AKI_Initial aki where scr.PATID = aki.PATID and scr.LAB_ORDER_DATE between aki.ADMIT_DATE_TIME and aki.DISCHARGE_DATE)


