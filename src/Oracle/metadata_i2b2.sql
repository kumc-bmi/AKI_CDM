/********************************************************************************/
/*@file metadata_cdm.sql
/*
/*in: pcornet_diag, pcornet_proc, pcornet_lab, pcornet_med
/*
/*params: &&cdm_db_schema
/*
/*out: metadata_cdm
/*
/*action: query
/********************************************************************************/
with cdm_meta_stk as (
select distinct 'DIAGNOSIS' TABLE_NAME
      ,'DX' FIELD_NAME
      ,PCORI_BASECODE VALUESET_ITEM
      ,regexp_substr(C_NAME,'[^\[]+',1,1) VALUESET_ITEM_DESCRIPTOR 
      ,C_HLEVEL I2B2_HLEVEL
      ,regexp_substr(C_FULLNAME,'[^\\]+',1,3) ITEM_TYPE
from &&cdm_db_schemametadata.pcornet_diag
where c_fullname like '\PCORI\%'
union all 
select distinct 'PROCEDURE' TABLE_NAME
      ,'PX' FIELD_NAME
      ,PCORI_BASECODE VALUESET_ITEM
      ,regexp_substr(C_NAME,'[^\[]+',1,1) VALUESET_ITEM_DESCRIPTOR 
      ,C_HLEVEL I2B2_HLEVEL
      ,regexp_substr(C_FULLNAME,'[^\\]+',1,3) ITEM_TYPE
from &&cdm_db_schemametadata.pcornet_proc
where c_fullname like '\PCORI\%'
union all
select distinct 'LAB_RESULT_CM' TABLE_NAME
      ,'LOINC' FIELD_NAME
      ,PCORI_BASECODE VALUESET_ITEM
      ,regexp_substr(C_NAME,'[^\[]+',1,1) VALUESET_ITEM_DESCRIPTOR
      ,C_HLEVEL I2B2_HLEVEL
      ,regexp_substr(C_FULLNAME,'[^\\]+',1,3) ITEM_TYPE
from &&cdm_db_schemametadata.pcornet_lab
where c_fullname like '\PCORI\%'
union all
select distinct 'PRESCRIBING' TABLE_NAME
      ,'RXNORM_CUI' as FIELD_NAME
      ,PCORI_BASECODE VALUESET_ITEM
      ,regexp_substr(C_NAME,'[^\[]+',1,1) VALUESET_ITEM_DESCRIPTOR
      ,C_HLEVEL I2B2_HLEVEL
      ,regexp_substr(C_FULLNAME,'[^\\]+',1,4) ITEM_TYPE
from &&cdm_db_schemametadata.pcornet_med
where c_fullname like '\PCORI\%' and 
      PCORI_CUI is not null
union all
select distinct 'DISPENSING' TABLE_NAME
      ,'NDC' FIELD_NAME
      ,C_BASECODE VALUESET_ITEM
      ,regexp_substr(C_NAME,'[^\[]+',1,1) VALUESET_ITEM_DESCRIPTOR
      ,C_HLEVEL I2B2_HLEVEL
      ,regexp_substr(C_FULLNAME,'[^\\]+',1,4) ITEM_TYPE
from &&cdm_db_schemametadata.pcornet_med
where c_fullname like '\PCORI\%' and 
      PCORI_NDC is not null
)
    ,rk_by_hlevel as (
select TABLE_NAME
      ,FIELD_NAME
      ,VALUESET_ITEM
      ,VALUESET_ITEM_DESCRIPTOR
      ,I2B2_HLEVEL
      ,ITEM_TYPE
      ,row_number() over (partition by TABLE_NAME, FIELD_NAME, VALUESET_ITEM order by I2B2_HLEVEL asc) rn
from cdm_meta_stk
where VALUESET_ITEM is not null
)
select TABLE_NAME
      ,FIELD_NAME
      ,VALUESET_ITEM
      ,VALUESET_ITEM_DESCRIPTOR
      ,I2B2_HLEVEL
      ,ITEM_TYPE
from rk_by_hlevel
where rn = 1


