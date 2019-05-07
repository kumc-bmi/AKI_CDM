/********************************************************************************/
/*@file consort_diagram.sql
/*
/*in: AKI_Initial,AKI_Scr_eGFR,exclude_all,AKI_onsets
/*
/*out: attrition
/*
/*action: query
/********************************************************************************/
select 'Initial' CNT_TYPE,
       count(distinct encounterid) ENC_CNT
from AKI_Initial
union all
select 'Has_at_least_2_SCr' CNT_TYPE,
       count(distinct encounterid) ENC_CNT
from AKI_Scr_eGFR
union all
select exclud_type CNT_TYPE,
       count(distinct encounterid) ENC_CNT
from exclude_all
group by exclud_type
union all
select 'Total' CNT_TYPE,
       count(distinct encounterid) ENC_CNT
from AKI_onsets
union all
select 'nonAKI' CNT_TYPE,
       count(distinct encounterid) ENC_CNT
from AKI_onsets
where AKI1_onset is null and
      AKI2_onset is null and
      AKI3_onset is null
union all
select 'AKI1' CNT_TYPE,
       count(distinct encounterid) ENC_CNT
from AKI_onsets
where AKI1_onset is not null
union all
select 'nonAKI_to_AKI2' CNT_TYPE,
       count(distinct encounterid) ENC_CNT
from AKI_onsets
where AKI1_onset is null and
      AKI2_onset is not null
union all
select 'AKI1_to_AKI2' CNT_TYPE,
       count(distinct encounterid) ENC_CNT
from AKI_onsets
where AKI1_onset is not null and
      AKI2_onset is not null
union all
select 'nonAKI_to_AKI3' CNT_TYPE,
       count(distinct encounterid) ENC_CNT
from AKI_onsets
where AKI1_onset is null and
      AKI2_onset is null and
      AKI3_onset is not null
union all
select 'nonAKI_to_AKI2_to_AKI3' CNT_TYPE,
       count(distinct encounterid) ENC_CNT
from AKI_onsets
where AKI1_onset is null and
      AKI2_onset is not null and
      AKI3_onset is not null
union all
select 'AKI1_to_AKI2_to_AKI3' CNT_TYPE,
       count(distinct encounterid) ENC_CNT
from AKI_onsets
where AKI1_onset is not null and
      AKI2_onset is not null and
      AKI3_onset is not null

