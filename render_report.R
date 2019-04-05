##################
# Render Reports #
##################

#clean up the slate
rm(list=ls())
gc()

#require libraries
source("./R/util.R")
require_libraries(c("knitr",
                    "rmarkdown"))


#===============generate report for Part I=========================#
#================ Cohort Extraction and QA ========================#
#render the rmarkdown report with the following expected outputs:
# 1. AKI_CDM_EXT_VALID_p1_QA.html
# 2. AKI_CDM_EXT_VALID_p1_QA_TBL.xlsx
render_report(which_report="./report/AKI_CDM_EXT_VALID_p1_QA.Rmd",
              DBMS_type="Oracle",
              driver_type="OCI",
              start_date="2010-01-01")

# The error: 
# Error in unlockBinding("params", <environment>) : no binding for "params"
# Can be ignored. It's because we need to delete "params" before the script is done.


#===============generate report for Part II.1 =========================#
#================ Benchmark Model Validation ==========================#
#render the rmarkdown report with the following expected outputs:
# 1. AKI_CDM_EXT_VALID_p2_1_Benchmark.html
# 2. AKI_CDM_EXT_VALID_p2_1_Benchmark_TBL.xlsx
render_report(which_report="./report/AKI_CDM_EXT_VALID_p2_1_Benchmark.Rmd")


#===============generate report for Part II.2 =========================#
#================ Benchmark Model Retraining ==========================#
#render the rmarkdown report with the following expected outputs:
# 1. AKI_CDM_EXT_VALID_p2_2_Retrain.html
# 2. AKI_CDM_EXT_VALID_p2_2_Retrain_TBL.xlsx
render_report(which_report="./report/AKI_CDM_EXT_VALID_p2_2_Retrain.Rmd")
