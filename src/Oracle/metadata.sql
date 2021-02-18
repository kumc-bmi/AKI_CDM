/********************************************************************************/
/*@file metadata.sql
/*
/*in: pcornet_diag, pcornet_proc, pcornet_lab, pcornet_med, concept_dimension
/*
/*params: &&cdm_meta_schema, &&i2b2_meta_schema
/*
/*out: metadata
/*
/*action: write
/********************************************************************************/
create table metadata as
with cdm_meta_stk as (
--select distinct 
--      'DIAGNOSIS' TABLE_NAME
--      ,'DX' FIELD_NAME
--      ,PCORI_BASECODE VALUESET_ITEM
--      ,regexp_substr(C_NAME,'[^\[]+',1,1) VALUESET_ITEM_DESCRIPTOR 
--      ,C_HLEVEL I2B2_HLEVEL
--      ,regexp_substr(C_FULLNAME,'[^\\]+',1,3) ITEM_TYPE
--from &&cdm_meta_schema.pcornet_diag
--where c_fullname like '\PCORI\%'
--union all 
--select distinct 
--      'DIAGNOSIS' TABLE_NAME
--      ,'DX' FIELD_NAME
--      ,regexp_substr(C_BASECODE,'[^:]+',1,2) VALUESET_ITEM
--      ,regexp_substr(C_NAME,'[^\[]+',1,1) VALUESET_ITEM_DESCRIPTOR 
--      ,C_HLEVEL I2B2_HLEVEL
--      ,case when regexp_substr(C_BASECODE,'[^:]+',1,1)='ICD9' then '09'
--            else replace(regexp_substr(C_BASECODE,'[^:]+',1,1),'ICD','') 
--       end as ITEM_TYPE
--from &&i2b2_meta_schema.heron_terms
--where c_fullname like '%Diagnos%ICD%' and c_basecode like 'ICD%'
--union all
select distinct
      'PROCEDURES' TABLE_NAME
      ,'PX' FIELD_NAME
      ,PCORI_BASECODE VALUESET_ITEM
      ,regexp_substr(C_NAME,'[^\[]+',1,1) VALUESET_ITEM_DESCRIPTOR 
      ,C_HLEVEL I2B2_HLEVEL
      ,regexp_substr(C_FULLNAME,'[^\\]+',1,3) ITEM_TYPE
from &&cdm_meta_schema.pcornet_proc
where c_fullname like '\PCORI\%'
union all 
select distinct 
      'PROCEDURES' TABLE_NAME
      ,'PX' FIELD_NAME
      ,regexp_substr(C_BASECODE,'[^:]+',1,2) VALUESET_ITEM
      ,regexp_substr(C_NAME,'[^\[]+',1,1) VALUESET_ITEM_DESCRIPTOR 
      ,C_HLEVEL I2B2_HLEVEL
      ,'CPT' ITEM_TYPE
from &&i2b2_meta_schema.heron_terms
where c_basecode like 'CPT%'
union all
select distinct
      'LAB_RESULT_CM' TABLE_NAME
      ,'LOINC' FIELD_NAME
      ,PCORI_BASECODE VALUESET_ITEM
      ,regexp_substr(C_NAME,'[^\[]+',1,1) VALUESET_ITEM_DESCRIPTOR
      ,C_HLEVEL I2B2_HLEVEL
      ,regexp_substr(C_FULLNAME,'[^\\]+',1,3) ITEM_TYPE
from &&cdm_meta_schema.pcornet_lab
where c_fullname like '\PCORI\%'
union all 
select distinct 
      'LAB_RESULT_CM' TABLE_NAME
      ,'LOINC' FIELD_NAME
      ,regexp_substr(C_BASECODE,'[^:]+',1,2) VALUESET_ITEM
      ,regexp_substr(C_NAME,'[^\[]+',1,1) VALUESET_ITEM_DESCRIPTOR 
      ,C_HLEVEL I2B2_HLEVEL
      ,'LOINC' ITEM_TYPE
from &&i2b2_meta_schema.heron_terms
where c_basecode like 'LOINC%'
union all
select distinct
      'PRESCRIBING' TABLE_NAME
      ,'RXNORM_CUI' as FIELD_NAME
      ,PCORI_CUI VALUESET_ITEM
      ,regexp_substr(C_NAME,'[^\[]+',1,1) VALUESET_ITEM_DESCRIPTOR
      ,C_HLEVEL I2B2_HLEVEL
      ,regexp_substr(C_BASECODE,'[^:]+',1,1) ITEM_TYPE
from &&cdm_meta_schema.pcornet_med
where c_fullname like '\PCORI\%' and 
      PCORI_CUI is not null and 
      C_BASECODE like 'RXNORM%'
union all
select distinct
      'DISPENSING' TABLE_NAME
      ,'NDC' FIELD_NAME
      ,PCORI_NDC VALUESET_ITEM
      ,regexp_substr(C_NAME,'[^\[]+',1,1) VALUESET_ITEM_DESCRIPTOR
      ,C_HLEVEL I2B2_HLEVEL
      ,regexp_substr(C_BASECODE,'[^:]+',1,1) ITEM_TYPE
from &&cdm_meta_schema.pcornet_med
where c_fullname like '\PCORI\%' and 
      PCORI_NDC is not null and 
      C_BASECODE like 'NDC%'
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
      ,ITEM_TYPE
from rk_by_hlevel
where rn = 1


