#source utility functions
source("./R/util.R")
source("./R/viz.R")

#load libraries
require_libraries(c("tidyr",
                    "dplyr",
                    "magrittr",
                    "stringr",
                    "knitr",
                    "openxlsx"))


wb_lst<-c("KUMC_AKI_CDM_EXT_VALID_p1_QA_TBL",
          "UTHSCA_AKI_CDM_EXT_VALID_p1_QA_TBL",
          "UIOWA_AKI_CDM_EXT_VALID_p1_QA_TBL")

# stack up consort diagram
consort<-c()
for (wb in seq_len(length(wb_lst))) {
  consort %<>%
    bind_rows(read.xlsx(paste0("./output/",wb_lst[wb],".xlsx"),sheet=1) %>%
                mutate(site=gsub("_.*","",wb_lst[wb])))
}


# stack up AKI times
aki_time<-c()
for (wb in seq_len(length(wb_lst))) {
  aki_time %<>%
    bind_rows(read.xlsx(paste0("./output/",wb_lst[wb],".xlsx"),sheet=2) %>%
                mutate(site=gsub("_.*","",wb_lst[wb])))
}


# stack up demo
demo<-c()
for (wb in seq_len(length(wb_lst))) {
  demo %<>%
    bind_rows(read.xlsx(paste0("./output/",wb_lst[wb],".xlsx"),sheet=3) %>%
                mutate(site=gsub("_.*","",wb_lst[wb])))
}


# stack up historical diagnosis



