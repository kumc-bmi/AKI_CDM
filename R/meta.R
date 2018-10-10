#### collect metadata ####
source("./R/util.R")

require_libraries(c("DBI",
                    "ROracle",
                    "tidyr",
                    "dplyr",
                    "magrittr"))

config_file_path<-"./config.csv"
config_file<-read.csv(config_file_path,stringsAsFactors = F)
conn<-connect_to_db("Oracle",config_file)

#metadata - source I: PCORNET CDM website


#metadata - source II: local metadata table
meta_sql<-parse_sql("./inst/Oracle/metadata_cdm.sql",
                    cdm_db_schema="blueheron")

ft_metadata<-execute_single_sql(conn,
                                statement=meta_sql$statement,
                                write=(meta_sql$action=="write"),
                                table_name=toupper(sql$tbl_out))

saveRDS(ft_metadata,file="./data/ft_metadata.rda")


#metadata - source III

