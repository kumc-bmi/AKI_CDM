/*******************************************************************************/
/*@file cohort_baseline_SCr.sql
/*
/*in: #AKI_Initial, #AKI_Scr_eGFR, #All_Scr_eGFR
/*       
/*out: #AKI_Scr_base
/*
/*action: write
/********************************************************************************/
--get first record within the encounter
with scr_enc1 as (
select scr.* from #AKI_Scr_eGFR scr
where scr.rn = 1
)
--get all historical Scr records within 2 days prior to encounter
    ,scr_prior as (
select scre1.PATID
      ,scre1.ENCOUNTERID
      ,scr.SERUM_CREAT
      ,scr.eGFR
      ,scr.LAB_ORDER_DATE
      ,scr.SPECIMEN_DATE_TIME
      ,scr.RESULT_DATE_TIME
      ,datediff(dd,scr.LAB_ORDER_DATE,scre1.LAB_ORDER_DATE) days_prior
      ,dense_rank() over (partition by scr.PATID order by abs(datediff(dd,scr.LAB_ORDER_DATE,scre1.LAB_ORDER_DATE))) rn_prior
from scr_enc1 scre1
join #All_Scr_eGFR scr
on scre1.PATID = scr.PATID
where scr.LAB_ORDER_DATE < scre1.LAB_ORDER_DATE and
      datediff(dd,scr.LAB_ORDER_DATE,scre1.LAB_ORDER_DATE)< 2 and -- within 2 days prior
      scre1.LAB_ORDER_DATE > scr.LAB_ORDER_DATE
)
--get the most recent historical Scr if there exists one
    ,scr_prior1 as (
select scrp1.* from scr_prior scrp1
where scrp1.rn_prior = 1
)
--put results together: 
---- if there exist some historical Scr before encounter X, use the most recent value (scr_prior1)
---- otherwise, use the very first record at the encoutner (scr_enc1)
   ,scr_base_dup as (
select distinct 
       s1.PATID
      ,s1.ENCOUNTERID
      ,coalesce(sp.SERUM_CREAT,s1.SERUM_CREAT) SERUM_CREAT
      ,coalesce(sp.eGFR, s1.eGFR) eGFR
      ,coalesce(sp.LAB_ORDER_DATE, s1.LAB_ORDER_DATE) LAB_ORDER_DATE
      ,coalesce(sp.SPECIMEN_DATE_TIME, s1.SPECIMEN_DATE_TIME) SPECIMEN_DATE_TIME
      ,coalesce(sp.RESULT_DATE_TIME, s1.RESULT_DATE_TIME) RESULT_DATE_TIME
      ,sp.days_prior
from scr_enc1 s1 
left join scr_prior1 sp
on s1.ENCOUNTERID = sp.ENCOUNTERID
order by s1.PATID, s1.ENCOUNTERID
)
--looks like there exists multiple historical values on the same day 
select scrb.PATID
      ,scrb.ENCOUNTERID
      ,init.ADMIT_DATE_TIME
      ,max(scrb.SERUM_CREAT) SERUM_CREAT
      ,min(scrb.eGFR) eGFR
      ,scrb.LAB_ORDER_DATE
      ,scrb.SPECIMEN_DATE_TIME
      ,scrb.RESULT_DATE_TIME
      ,scrb.days_prior
from scr_base_dup scrb
join #AKI_Initial init
into #AKI_Scr_base
on scrb.ENCOUNTERID = init.ENCOUNTERID
group by scrb.PATID,scrb.ENCOUNTERID,init.ADMIT_DATE_TIME,scrb.LAB_ORDER_DATE,scrb.SPECIMEN_DATE_TIME,scrb.RESULT_DATE_TIME,scrb.days_prior
