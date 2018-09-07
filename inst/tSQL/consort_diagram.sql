/*********************************************************/
/* collect attrition information
/*********************************************************/
select 'Initial' cnt_type,
       count(distinct encounterid) enc_cnt
from #AKI_Initial
union all
select 'Has_at_least_1_SCr' cnt_type,
       count(distinct encounterid) enc_cnt
from #AKI_Scr_eGFR
union all
select exclud_type,
       count(distinct encounterid) enc_cnt
from #exclude_all
group by exclud_type
union all
select 'Total' cnt_type,
       count(distinct encounterid) enc_cnt
from #AKI_onsets
union all
select 'nonAKI' cnt_type,
       count(distinct encounterid) enc_cnt
from #AKI_onsets
where AKI1_onset is null and
      AKI2_onset is null and
      AKI3_onset is null
union all
select 'AKI1' cnt_type,
       count(distinct encounterid) enc_cnt
from #AKI_onsets
where AKI1_onset is not null
union all
select 'nonAKI_to_AKI2' cnt_type,
       count(distinct encounterid) enc_cnt
from #AKI_onsets
where AKI1_onset is null and
      AKI2_onset is not null
union all
select 'AKI1_to_AKI2' cnt_type,
       count(distinct encounterid) enc_cnt
from #AKI_onsets
where AKI1_onset is not null and
      AKI2_onset is not null
union all
select 'nonAKI_to_AKI3' cnt_type,
       count(distinct encounterid) enc_cnt
from #AKI_onsets
where AKI1_onset is null and
      AKI2_onset is null and
      AKI3_onset is not null
union all
select 'nonAKI_to_AKI2_to_AKI3' cnt_type,
       count(distinct encounterid) enc_cnt
from #AKI_onsets
where AKI1_onset is null and
      AKI2_onset is not null and
      AKI3_onset is not null
union all
select 'AKI1_to_AKI2_to_AKI3' cnt_type,
       count(distinct encounterid) enc_cnt
from #AKI_onsets
where AKI1_onset is not null and
      AKI2_onset is not null and
      AKI3_onset is not null

