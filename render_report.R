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
#render the rmarkdown report with the following expected outputs:
# 1. AKI_CDM_EXT_VALID_p1_QA.html
# 2. AKI_CDM_EXT_VALID_p1_QA_TBL.xlsx
render_report(which_report="./report/AKI_CDM_EXT_VALID_p1_QA.Rmd",
              DBMS_type="Oracle")

# The error: 
# Error in unlockBinding("params", <environment>) : no binding for "params"
# Can be ignored. It's because we need to delete "params" before the script is done.


#TODO:
#===============generate report for Part II=========================#
# render_report(path_to_input="./report/AKI_CDM_EXT_VALID_p2_ModelDev.Rmd",
#               DBMS_type="Oracle")